package nemosofts.streambox.activity;

import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.KeyEvent;
import android.view.View;
import android.view.animation.LinearInterpolator;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;

import androidx.activity.EdgeToEdge;
import androidx.annotation.NonNull;
import androidx.annotation.StringRes;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.Toasty;

import androidx.nemosofts.utils.DeviceUtils;
import androidx.nemosofts.utils.NetworkUtils;

import com.example.internet_speed_testing.InternetSpeedBuilder;
import com.example.internet_speed_testing.ProgressionModel;

import java.text.DecimalFormat;

import nemosofts.streambox.BuildConfig;
import nemosofts.streambox.R;
import nemosofts.streambox.utils.IfSupported;
import nemosofts.streambox.utils.helper.ThemeHelper;

public class NetworkSpeedActivity extends AppCompatActivity {

    private static final long SPEED_TEST_DELAY_MS = 500L;
    private static final float MAX_GAUGE_SPEED_MBPS = 100f;
    private static final int MAX_GAUGE_ROTATION = 240;
    private static final DecimalFormat SPEED_DECIMAL = new DecimalFormat("0.00");

    private final Handler mainHandler = new Handler(Looper.getMainLooper());
    private final Runnable startSpeedRunnable = this::startSpeedMeasurement;

    private final InternetSpeedBuilder.OnEventInternetSpeedListener speedTestListener =
            new InternetSpeedBuilder.OnEventInternetSpeedListener() {
                @Override
                public void onDownloadProgress(int count, @NonNull final ProgressionModel progressModel) {
                    final double download = toMbps(progressModel.getDownloadSpeed().doubleValue());
                    lastDownloadMbps = download;
                    runOnUiThread(() -> {
                        updateGaugeRotation((float) download, true);
                        updateDownloadResult(download);
                    });
                }

                @Override
                public void onUploadProgress(int count, @NonNull final ProgressionModel progressModel) {
                    final double upload = toMbps(progressModel.getUploadSpeed().doubleValue());
                    runOnUiThread(() -> updateUploadResult(upload, lastDownloadMbps));
                }

                @Override
                public void onTotalProgress(int count, @NonNull final ProgressionModel progressModel) {
                    final double download = toMbps(progressModel.getDownloadSpeed().doubleValue());
                    final double upload = toMbps(progressModel.getUploadSpeed().doubleValue());
                    lastDownloadMbps = download;
                    final float average = (float) ((download + upload) / 2f);
                    runOnUiThread(() -> {
                        updateGaugeRotation(average, true);
                        updateUploadResult(upload, download);
                        maybeFinishTest(progressModel);
                    });
                }
            };

    private double lastDownloadMbps = 0d;
    private SpeedTestState speedTestState;
    private InternetSpeedBuilder internetSpeedBuilder;

    private TextView downloadSpeed;
    private TextView totalSpeed;
    private ImageView barImage;
    private LinearLayout btnSpeed;
    private ImageView speedIcon;
    private View speedProgress;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        super.onCreate(savedInstanceState);
        IfSupported.keepScreenOn(this);
        EdgeToEdge.enable(this);
        ViewCompat.setOnApplyWindowInsetsListener(findViewById(R.id.theme_bg), (v, insets) -> {
            Insets systemInsets = insets.getInsets(WindowInsetsCompat.Type.systemBars());
            v.setPadding(systemInsets.left, systemInsets.top, systemInsets.right, systemInsets.bottom);
            return insets;
        });

        initializeUI();
    }

    private void initializeUI() {
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ThemeHelper.getThemeBackgroundRes(this));

        findViewById(R.id.iv_back_page).setOnClickListener(view -> finish());
        if (DeviceUtils.isTvBox(this)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        btnSpeed = findViewById(R.id.ll_btn_speed);

        barImage =  findViewById(R.id.barImageView);
        downloadSpeed = findViewById(R.id.download);
        totalSpeed = findViewById(R.id.total_speed);
        speedIcon = findViewById(R.id.tv_speed);
        speedProgress = findViewById(R.id.pb_speed);

        btnSpeed.setOnClickListener(v -> runSpeedTest());
        if (DeviceUtils.isTvBox(this)){
            btnSpeed.requestFocus();
        }
        setSpeedTestState(SpeedTestState.IDLE);
    }

    private void runSpeedTest() {
        if (speedTestState == SpeedTestState.PREPARING || speedTestState == SpeedTestState.RUNNING) {
            Toasty.makeText(this, getString(R.string.speed_test_in_progress), Toasty.INFO);
            return;
        }
        if (!NetworkUtils.isConnected(this)){
            handleSpeedTestFailure(R.string.err_internet_not_connected);
            return;
        }

        lastDownloadMbps = 0d;
        updateGaugeRotation(0f, false);
        setSpeedTestState(SpeedTestState.PREPARING);
        mainHandler.removeCallbacks(startSpeedRunnable);
        mainHandler.postDelayed(startSpeedRunnable, SPEED_TEST_DELAY_MS);
    }

    @NonNull
    public static String formatSpeed(double speedInMbps) {
        if (speedInMbps <= 0) {
            return "0 Mbps";
        }
        if (speedInMbps >= 1000) {
            return SPEED_DECIMAL.format(speedInMbps / 1000d) + " Gbps";
        }
        return SPEED_DECIMAL.format(speedInMbps) + " Mbps";
    }

    public int getPositionByRate(float rate) {
        float safeRate = Math.max(0f, Math.min(rate, MAX_GAUGE_SPEED_MBPS));
        if (safeRate <= 1f) {
            return (int) (safeRate * 30);
        } else if (safeRate <= 10f) {
            return (int) (safeRate * 6) + 30;
        } else if (safeRate <= 30f) {
            return (int) ((safeRate - 10f) * 3f) + 90;
        } else if (safeRate <= 50f) {
            return (int) ((safeRate - 30f) * 1.5f) + 150;
        } else if (safeRate <= 100f) {
            return (int) ((safeRate - 50f) * 1.2f) + 180;
        }
        return MAX_GAUGE_ROTATION;
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_network_speed;
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN &&  keyCode == KeyEvent.KEYCODE_BACK){
            finish();
            return true;
        } else if (event.getAction() == KeyEvent.ACTION_DOWN &&  keyCode == KeyEvent.KEYCODE_HOME){
            ThemeHelper.openHomeActivity(this);
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        mainHandler.removeCallbacks(startSpeedRunnable);
        if (internetSpeedBuilder != null) {
            internetSpeedBuilder = null;
        }
    }

    private void startSpeedMeasurement() {
        if (isFinishing()) {
            return;
        }
        if (!NetworkUtils.isConnected(this)) {
            handleSpeedTestFailure(R.string.err_internet_not_connected);
            return;
        }
        ensureSpeedBuilder();
        downloadSpeed.setVisibility(View.VISIBLE);
        totalSpeed.setVisibility(View.VISIBLE);
        setSpeedTestState(SpeedTestState.RUNNING);
        internetSpeedBuilder.start(BuildConfig.BASE_URL + "uploads/1Mo.dat", 1);
    }

    private void ensureSpeedBuilder() {
        if (internetSpeedBuilder == null) {
            internetSpeedBuilder = new InternetSpeedBuilder(this);
            internetSpeedBuilder.setOnEventInternetSpeedListener(speedTestListener);
        }
    }

    private void updateGaugeRotation(float speedMbps, boolean animate) {
        int targetPosition = getPositionByRate(speedMbps);
        if (animate) {
            barImage.animate()
                    .rotation(targetPosition)
                    .setDuration(500)
                    .setInterpolator(new LinearInterpolator())
                    .start();
        } else {
            barImage.setRotation(targetPosition);
        }
    }

    public void updateDownloadResult(double downloadSpeedValue) {
        if (downloadSpeed == null) {
            return;
        }
        downloadSpeed.setText(getString(R.string.speed_download_label, formatSpeed(downloadSpeedValue)));
    }

    public void updateUploadResult(double uploadSpeedValue, double downloadSpeedValue) {
        if (totalSpeed == null) {
            return;
        }
        double average = downloadSpeedValue > 0 ? (downloadSpeedValue + uploadSpeedValue) / 2d : uploadSpeedValue;
        String uploadLine = getString(R.string.speed_upload_label, formatSpeed(uploadSpeedValue));
        String averageLine = getString(R.string.speed_average_label, formatSpeed(average));
        totalSpeed.setText(getString(R.string.speed_summary_template, uploadLine, averageLine));
    }

    public void maybeFinishTest(@NonNull ProgressionModel progressModel) {
        if (progressModel.getProgressTotal() >= 100f) {
            onSpeedTestCompleted();
        }
    }

    private void onSpeedTestCompleted() {
        setSpeedTestState(SpeedTestState.FINISHED);
        if (DeviceUtils.isTvBox(this)) {
            btnSpeed.post(btnSpeed::requestFocus);
        }
    }

    private void handleSpeedTestFailure(@StringRes int messageRes) {
        Toasty.makeText(NetworkSpeedActivity.this, getString(messageRes), Toasty.ERROR);
        setSpeedTestState(SpeedTestState.ERROR);
    }

    private void setSpeedTestState(@NonNull SpeedTestState newState) {
        if (speedTestState == newState) {
            return;
        }
        speedTestState = newState;
        switch (newState) {
            case IDLE:
                btnSpeed.setVisibility(View.VISIBLE);
                btnSpeed.setEnabled(true);
                speedIcon.setVisibility(View.VISIBLE);
                speedProgress.setVisibility(View.GONE);
                downloadSpeed.setVisibility(View.GONE);
                totalSpeed.setVisibility(View.GONE);
                break;
            case PREPARING:
                btnSpeed.setVisibility(View.VISIBLE);
                btnSpeed.setEnabled(false);
                speedIcon.setVisibility(View.GONE);
                speedProgress.setVisibility(View.VISIBLE);
                break;
            case RUNNING:
                btnSpeed.setVisibility(View.GONE);
                downloadSpeed.setVisibility(View.VISIBLE);
                totalSpeed.setVisibility(View.VISIBLE);
                break;
            case FINISHED:
                btnSpeed.setVisibility(View.VISIBLE);
                btnSpeed.setEnabled(true);
                speedIcon.setVisibility(View.VISIBLE);
                speedProgress.setVisibility(View.GONE);
                break;
            case ERROR:
                btnSpeed.setVisibility(View.VISIBLE);
                btnSpeed.setEnabled(true);
                speedIcon.setVisibility(View.VISIBLE);
                speedProgress.setVisibility(View.GONE);
                downloadSpeed.setVisibility(View.GONE);
                totalSpeed.setVisibility(View.GONE);
                if (DeviceUtils.isTvBox(this)) {
                    btnSpeed.requestFocus();
                }
                break;
        }
    }

    static double toMbps(double speed) {
        return speed / 1_000_000d;
    }

    private enum SpeedTestState {
        IDLE,
        PREPARING,
        RUNNING,
        FINISHED,
        ERROR
    }
}