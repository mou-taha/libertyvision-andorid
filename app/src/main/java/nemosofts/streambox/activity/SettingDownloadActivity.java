package nemosofts.streambox.activity;

import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.KeyEvent;
import android.view.View;
import android.widget.CheckBox;
import android.widget.ImageView;
import android.widget.TextView;

import androidx.activity.EdgeToEdge;
import androidx.annotation.NonNull;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.Toasty;
import androidx.nemosofts.utils.DeviceUtils;

import nemosofts.streambox.R;
import nemosofts.streambox.utils.IfSupported;
import nemosofts.streambox.utils.helper.SPHelper;
import nemosofts.streambox.utils.helper.ThemeHelper;

public class SettingDownloadActivity extends AppCompatActivity {

    private CheckBox switchWifi;
    private CheckBox switchMobile;
    private TextView summary;
    SPHelper spHelper;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        super.onCreate(savedInstanceState);
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

        View root = findViewById(R.id.theme_bg);
        root.setBackgroundResource(ThemeHelper.getThemeBackgroundRes(this));

        ImageView back = findViewById(R.id.iv_back_page);
        back.setOnClickListener(v -> finish());
        if (DeviceUtils.isTvBox(this)) {
            back.setVisibility(View.GONE);
        }

        spHelper = new SPHelper(this);
        summary = findViewById(R.id.tv_download_network_summary);

        switchWifi = findViewById(R.id.switch_wifi);
        switchWifi.setChecked(spHelper.isDownloadWifiAllowed());

        switchMobile = findViewById(R.id.switch_mobile);
        switchMobile.setChecked(spHelper.isDownloadMobileAllowed());

        findViewById(R.id.ll_btn_save).setOnClickListener(v -> {
            findViewById(R.id.tv_save).setVisibility(View.GONE);
            findViewById(R.id.pb_save).setVisibility(View.VISIBLE);
            spHelper.setDownloadWifiAllowed(switchWifi.isChecked());
            spHelper.setDownloadMobileAllowed(switchMobile.isChecked());
            updateSummary();
            new Handler(Looper.getMainLooper()).postDelayed(() -> {
                findViewById(R.id.tv_save).setVisibility(View.VISIBLE);
                findViewById(R.id.pb_save).setVisibility(View.GONE);
                Toasty.makeText(SettingDownloadActivity.this,"Save Data", Toasty.SUCCESS);
            }, 500);
        });

        updateSummary();
    }

    private void updateSummary() {
        boolean wifi = switchWifi.isChecked();
        boolean mobile = switchMobile.isChecked();
        int textRes;
        if (wifi && mobile) {
            textRes = R.string.download_network_summary_any;
        } else if (wifi) {
            textRes = R.string.download_network_summary_wifi_only;
        } else if (mobile) {
            textRes = R.string.download_network_summary_mobile_only;
        } else {
            textRes = R.string.download_network_summary_blocked;
        }
        summary.setText(textRes);
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_setting_download;
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && keyCode == KeyEvent.KEYCODE_BACK) {
            finish();
            return true;
        } else if (event.getAction() == KeyEvent.ACTION_DOWN && keyCode == KeyEvent.KEYCODE_HOME) {
            ThemeHelper.openHomeActivity(this);
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }
}