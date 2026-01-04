package nemosofts.streambox.activity;

import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.res.Configuration;
import android.net.Uri;
import android.os.Bundle;
import android.view.KeyEvent;
import android.view.View;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import androidx.activity.EdgeToEdge;
import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.appcompat.app.AlertDialog;
import androidx.appcompat.app.AppCompatActivity;
import androidx.media3.common.MediaItem;
import androidx.media3.common.PlaybackException;
import androidx.media3.common.Player;
import androidx.media3.common.util.UnstableApi;
import androidx.media3.common.util.Util;
import androidx.media3.datasource.DataSource;
import androidx.media3.datasource.DefaultDataSource;
import androidx.media3.datasource.DefaultHttpDataSource;
import androidx.media3.exoplayer.ExoPlayer;
import androidx.media3.exoplayer.source.MediaSource;
import androidx.media3.exoplayer.source.ProgressiveMediaSource;
import androidx.nemosofts.LauncherListener;
import androidx.nemosofts.LauncherTask;
import androidx.nemosofts.theme.ColorUtils;
import androidx.nemosofts.theme.ThemeEngine;

import androidx.nemosofts.utils.DeviceUtils;
import androidx.nemosofts.utils.NetworkUtils;

import nemosofts.streambox.BuildConfig;
import nemosofts.streambox.R;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.executor.LoadAbout;
import nemosofts.streambox.executor.LoadData;
import nemosofts.streambox.interfaces.AboutListener;
import nemosofts.streambox.interfaces.DataListener;
import nemosofts.streambox.utils.IfSupported;
import nemosofts.streambox.utils.helper.Helper;
import nemosofts.streambox.utils.helper.SPHelper;
import nemosofts.streambox.utils.helper.ThemeHelper;

public class LauncherActivity extends AppCompatActivity implements LauncherListener {

    Helper helper;
    SPHelper spHelper;
    private ProgressBar pb;
    private ExoPlayer exoPlayer = null;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        super.onCreate(savedInstanceState);
        EdgeToEdge.enable(this);
        setContentView(R.layout.activity_launcher);
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);
        IfSupported.hideStatusBar(this);
        IfSupported.keepScreenOn(this);

        helper = new Helper(this);
        spHelper = new SPHelper(this);

        ThemeHelper.applyTheme(this, spHelper, findViewById(R.id.theme_bg));

        TextView appVersion = findViewById(R.id.tv_version);
        appVersion.setTextColor(ColorUtils.colorTitleSub(this));
        String version = getString(R.string.version) + " " + BuildConfig.VERSION_NAME;
        appVersion.setText(version);

        pb = findViewById(R.id.pb_splash);

        prepareAudio();
        loadAboutData();
    }

    private void loadAboutData() {
        if (!NetworkUtils.isConnected(this)){
            if (Boolean.TRUE.equals(spHelper.getIsAboutDetails())){
                setSaveData();
            } else {
                errorDialog(getString(R.string.err_internet_not_connected), getString(R.string.err_connect_net_try));
            }
            return;
        }

        new LoadAbout(LauncherActivity.this, new AboutListener() {
            @Override
            public void onStart() {
                pb.setVisibility(View.VISIBLE);
            }

            @Override
            public void onEnd(String success, String verifyStatus, String message){
                if (isFinishing()){
                    return;
                }

                pb.setVisibility(View.GONE);
                if (success.equals("1")){
                    setSaveData();
                } else {
                    if (Boolean.TRUE.equals(spHelper.getIsAboutDetails())){
                        setSaveData();
                    } else {
                        errorDialog(getString(R.string.err_server_error), getString(R.string.err_server_not_connected));
                    }
                }
            }
        }).execute();
    }

    @OptIn(markerClass = UnstableApi.class)
    private void prepareAudio() {
        if (Boolean.TRUE.equals(spHelper.getIsSplashAudio())){
            exoPlayer = new ExoPlayer.Builder(this).build();
            DataSource.Factory dataSourceFactory = new DefaultDataSource.Factory(this,
                    new DefaultHttpDataSource.Factory().setUserAgent(Util.getUserAgent(this, "nemosofts_rc")));
            Uri fileUri = Uri.parse("android.resource://" + getPackageName() + "/" + R.raw.opener_logo);
            MediaSource mediaSource = new ProgressiveMediaSource.Factory(dataSourceFactory).createMediaSource(MediaItem.fromUri(fileUri));
            exoPlayer.setMediaSource(mediaSource);
            exoPlayer.prepare();
            exoPlayer.setPlayWhenReady(false);
            exoPlayer.addListener(new Player.Listener() {

                @Override
                public void onPlaybackStateChanged(int state) {
                    Player.Listener.super.onPlaybackStateChanged(state);
                    if (state == Player.STATE_ENDED){
                        handlePlaybackCompletion();
                    }
                }

                @Override
                public void onPlayerError(@NonNull PlaybackException error) {
                    Player.Listener.super.onPlayerError(error);
                    handlePlaybackCompletion();
                }
            });
        }
    }

    private void handlePlaybackCompletion() {
        if (!isFinishing()) {
            loadSettings();
        }
    }

    private void playAudio() {
        if (Boolean.FALSE.equals(spHelper.getIsSplashAudio())){
            loadSettings();
        } else if (exoPlayer != null){
            exoPlayer.play();
        } else {
            loadSettings();
        }
    }

    private void loadSettings() {
        if (Boolean.FALSE.equals(spHelper.getIsAboutDetails())){
            new ThemeEngine(this).setDark(true);
            spHelper.setIsAutoStart(DeviceUtils.isTvBox(this));
            spHelper.setAboutDetails(true);
        }
        if (Boolean.TRUE.equals(Callback.getIsAppUpdate()) && Callback.getAppNewVersion() != BuildConfig.VERSION_CODE){
            openDialogActivity(Callback.DIALOG_TYPE_UPDATE);
        } else if(Boolean.TRUE.equals(spHelper.getIsMaintenance())) {
            openDialogActivity(Callback.DIALOG_TYPE_MAINTENANCE);
        } else if(Boolean.TRUE.equals(spHelper.getIsAPK()) && IfSupported.isDeveloperModeEnabled(this)){
            openDialogActivity(Callback.DIALOG_TYPE_DEVELOPER);
        } else if(Boolean.TRUE.equals(spHelper.getIsVPN()) && IfSupported.isVPNActive(this)){
            openDialogActivity(Callback.DIALOG_TYPE_VPN);
        } else {
            handleLoginType();
        }
    }

    private void handleLoginType() {
        String loginType = spHelper.getLoginType();
        if (Callback.TAG_LOGIN_SINGLE_STREAM.equals(loginType)) {
            startActivity(SingleStreamActivity.class);
        } else if (Callback.TAG_LOGIN_VIDEOS.equals(loginType)) {
            startActivity(LocalStorageActivity.class);
        } else if (Callback.TAG_LOGIN_PLAYLIST.equals(loginType)) {
            startActivity(PlaylistActivity.class);
        } else if (isStreamOrOneUILogin(loginType)) {
            handleStreamLogin();
        } else {
            openSelectPlayer();
        }
    }

    private boolean isStreamOrOneUILogin(String loginType) {
        return Callback.TAG_LOGIN_ONE_UI.equals(loginType) || Callback.TAG_LOGIN_STREAM.equals(loginType);
    }

    private void handleStreamLogin() {
        if (Boolean.TRUE.equals(spHelper.getIsFirst()) || Boolean.FALSE.equals(spHelper.getIsAutoLogin())) {
            openSelectPlayer();
        } else {
            getData();
        }
    }

    private void getData() {
        if (isFinishing()){
            return;
        }

        if (!NetworkUtils.isConnected(this)){
            ThemeHelper.openThemeActivity(LauncherActivity.this);
            return;
        }

        new LoadData(LauncherActivity.this, new DataListener() {
            @Override
            public void onStart() {
                pb.setVisibility(View.VISIBLE);
            }

            @Override
            public void onEnd(String success) {
                if (isFinishing()){
                    return;
                }
                pb.setVisibility(View.GONE);

                ThemeHelper.openThemeActivity(LauncherActivity.this);
            }
        }).execute();
    }

    @OptIn(markerClass = UnstableApi.class)
    private void openSelectPlayer() {
        Bundle extras = new Bundle();
        extras.putString("from", "");
        startActivity(SelectPlayerActivity.class, extras);
    }

    private void openDialogActivity(String type) {
        Bundle extras = new Bundle();
        extras.putString("from", type);
        startActivity(DialogActivity.class, extras);
    }

    private void startActivity(Class<?> activityClass) {
        startActivity(activityClass, null);
    }

    private void startActivity(Class<?> activityClass, Bundle extras) {
        if (spHelper != null){
            int orientation = spHelper.getOrientation();
            if (getResources().getConfiguration().orientation == Configuration.ORIENTATION_PORTRAIT && orientation == Callback.TAG_ORIENTATION_LANDSCAPE){
                setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
                return;
            }
            if (getResources().getConfiguration().orientation == Configuration.ORIENTATION_LANDSCAPE && orientation == Callback.TAG_ORIENTATION_PORTRAIT){
                setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_UNSPECIFIED);
                return;
            }
        }

        Intent intent = new Intent(LauncherActivity.this, activityClass);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        if (extras != null) {
            intent.putExtras(extras);
        }
        startActivity(intent);
        finish();
    }

    @Override
    public void onStartPairing() {
        pb.setVisibility(View.VISIBLE);
    }

    @Override
    public void onConnected() {
        pb.setVisibility(View.GONE);
        playAudio();
    }

    @Override
    public void onError(String message) {
        pb.setVisibility(View.GONE);
        if (message == null || message.isEmpty()){
            errorDialog(getString(R.string.err_server_error), "Server not connected lib");
            return;
        }
        errorDialog(getString(R.string.err_unauthorized_access), message);
    }

    private void errorDialog(String title, String message) {
        if (isFinishing()) {
            return;
        }

        runOnUiThread(() -> {
            AlertDialog.Builder builder = new AlertDialog.Builder(this, R.style.ThemeDialog)
                    .setTitle(title)
                    .setMessage(message)
                    .setCancelable(false)
                    .setPositiveButton(getString(R.string.exit), (d, w) -> finish());

            if (title.equals(getString(R.string.err_internet_not_connected))) {
                builder.setNegativeButton(getString(R.string.retry), (d, w) -> loadSettings());
            }

            builder.show();
            Toast.makeText(this, message, Toast.LENGTH_SHORT).show();
        });
    }

    private void setSaveData() {
        new LauncherTask(LauncherActivity.this, LauncherActivity.this).execute();
    }

    @Override
    protected void onDestroy() {
        releasePlayer();
        super.onDestroy();
    }

    private void releasePlayer() {
        if (exoPlayer != null) {
            exoPlayer.release();
            exoPlayer = null;
        }
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && (keyCode == KeyEvent.KEYCODE_BACK)){
            finish();
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }
}