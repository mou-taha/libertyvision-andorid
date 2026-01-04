package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.app.AppOpsManager;
import android.app.PictureInPictureParams;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.IntentFilter;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.content.res.Configuration;
import android.media.AudioManager;
import android.media.audiofx.AudioEffect;
import android.media.audiofx.LoudnessEnhancer;
import android.media.metrics.PlaybackStateEvent;
import android.net.Uri;
import android.os.BatteryManager;
import android.os.Build;
import android.os.Bundle;
import android.os.CountDownTimer;
import android.os.Handler;
import android.os.Looper;
import android.util.Rational;
import android.view.KeyEvent;
import android.view.SurfaceView;
import android.view.View;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.TextView;
import android.widget.Toast;

import androidx.activity.EdgeToEdge;
import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatDelegate;
import androidx.media3.common.AudioAttributes;
import androidx.media3.common.C;
import androidx.media3.common.Format;
import androidx.media3.common.MediaItem;
import androidx.media3.common.PlaybackException;
import androidx.media3.common.Player;
import androidx.media3.common.TrackSelectionParameters;
import androidx.media3.common.Tracks;
import androidx.media3.common.VideoSize;
import androidx.media3.common.util.UnstableApi;
import androidx.media3.common.util.Util;
import androidx.media3.datasource.DataSource;
import androidx.media3.datasource.DefaultDataSource;
import androidx.media3.datasource.DefaultHttpDataSource;
import androidx.media3.datasource.HttpDataSource;
import androidx.media3.exoplayer.DefaultLoadControl;
import androidx.media3.exoplayer.DefaultRenderersFactory;
import androidx.media3.exoplayer.ExoPlayer;
import androidx.media3.exoplayer.dash.DashMediaSource;
import androidx.media3.exoplayer.dash.DefaultDashChunkSource;
import androidx.media3.exoplayer.hls.HlsMediaSource;
import androidx.media3.exoplayer.rtsp.RtspMediaSource;
import androidx.media3.exoplayer.smoothstreaming.DefaultSsChunkSource;
import androidx.media3.exoplayer.smoothstreaming.SsMediaSource;
import androidx.media3.exoplayer.source.DefaultMediaSourceFactory;
import androidx.media3.exoplayer.source.MediaSource;
import androidx.media3.exoplayer.source.ProgressiveMediaSource;
import androidx.media3.exoplayer.trackselection.DefaultTrackSelector;
import androidx.media3.exoplayer.upstream.DefaultBandwidthMeter;
import androidx.media3.extractor.DefaultExtractorsFactory;
import androidx.media3.session.MediaSession;
import androidx.media3.ui.AspectRatioFrameLayout;
import androidx.media3.ui.PlayerView;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.Toasty;

import androidx.nemosofts.utils.DeviceUtils;
import androidx.nemosofts.utils.EncrypterUtils;
import androidx.nemosofts.utils.FormatUtils;
import androidx.nemosofts.utils.NetworkUtils;
import androidx.nemosofts.utils.PlayerAPI;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.net.CookieHandler;
import java.net.CookieManager;
import java.net.CookiePolicy;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Locale;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterChannelPlayer;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.executor.LoadEpg;
import nemosofts.streambox.interfaces.EpgListener;
import nemosofts.streambox.item.ItemEpg;
import nemosofts.streambox.utils.ApplicationUtil;
import nemosofts.streambox.utils.IfSupported;
import nemosofts.streambox.utils.helper.DBHelper;
import nemosofts.streambox.utils.helper.Helper;
import nemosofts.streambox.utils.helper.SPHelper;
import nemosofts.streambox.utils.helper.VolumeHelper;
import nemosofts.streambox.utils.player.AudioTracksUtils;
import nemosofts.streambox.utils.player.BrightnessVolumeControl;
import nemosofts.streambox.utils.player.CustomPlayerView;
import nemosofts.streambox.utils.player.PlayerUtils;
import nemosofts.streambox.utils.player.VideoTracksUtils;

@UnstableApi
public class ExoPlayerLiveActivity extends AppCompatActivity {

    private static final String TAG = "PlayerLiveActivity";
    private static final long LOCK_HINT_TIMEOUT_MS = 3000L;
    private Helper helper;
    private DBHelper dbHelper;
    private SPHelper spHelper;

    private PlayerListener playerListener;
    private MediaSession mediaSession;
    private LoudnessEnhancer loudnessEnhancer;
    private BroadcastReceiver batteryReceiver;
    private AudioManager mAudioManager;
    private int boostLevel = 0;
    private boolean isTvBox;
    private int resize = 1;

    private CustomPlayerView playerView;
    private ExoPlayer exoPlayer;
    private DefaultBandwidthMeter bandwidthMeter;
    private DataSource.Factory mediaDataSourceFactory;
    private ProgressBar loadingProgressBar;

    private int playback = 0;

    private ImageView btnPlay;
    private ImageView btnFav;
    private ImageView btnSubtitle;
    private ImageView btnTryAgain;
    private TextView playerTitle;
    private LinearLayout llEpg;
    private TextView epgTitle;
    private TextView epgTime;
    private ImageView previous;
    private ImageView next;

    // VideoTracks
    private ImageView btnVideoTracks;

    // AudioTracks
    private ImageView btnAudioTracks;

    // RewardAd
    private CountDownTimer countDownTimer;
    private long timeLeftInMillis;

    private boolean controllerVisible;
    private boolean controllerVisibleFully;
    private boolean isPlayerLocked = false;
    private View lockOverlay;
    private View lockOverlayContent;
    private ImageView lockToggleButton;
    private ImageView unlockPlayerButton;
    private final Runnable hideLockOverlayRunnable = this::hideLockOverlayHint;

    private boolean isInPicInPicMode = false;
    private boolean isVisibleDialogInfo = false;
    private boolean isVisibleDialogList = false;
    private final Handler liveClockHandler = new Handler(Looper.getMainLooper());
    private final Runnable liveClockRunnable = new Runnable() {
        @Override
        public void run() {
            updateLiveClock();
            liveClockHandler.postDelayed(this, 60_000);
        }
    };
    private TextView liveClockView;
    private TextView liveResolutionView;

    private static final CookieManager DEFAULT_COOKIE_MANAGER;
    static {
        DEFAULT_COOKIE_MANAGER = new CookieManager();
        DEFAULT_COOKIE_MANAGER.setCookiePolicy(CookiePolicy.ACCEPT_ORIGINAL_SERVER);
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        super.onCreate(savedInstanceState);
        EdgeToEdge.enable(this);
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);
        IfSupported.hideStatusBar(this);
        IfSupported.playerHideStatusBar(this);

        if (Build.VERSION.SDK_INT >= 35) {
            getWindow().setNavigationBarContrastEnforced(false);
        }

        isTvBox = DeviceUtils.isTvBox(this);

        timeLeftInMillis = Callback.getRewardMinutes() * 60 * 1000; // minutes in milliseconds

        helper = new Helper(this);
        spHelper = new SPHelper(this);
        dbHelper = new DBHelper(this);

        playerTitle = findViewById(R.id.tv_player_title);
        liveResolutionView = findViewById(R.id.tv_live_resolution);
        liveClockView = findViewById(R.id.tv_live_clock);
        updateLiveHeaderInfo();

        loadingProgressBar = findViewById(R.id.pb_player);
        btnTryAgain = findViewById(R.id.iv_reset);
        btnPlay = findViewById(R.id.iv_play);
        btnFav = findViewById(R.id.iv_player_fav);
        btnSubtitle = findViewById(R.id.exo_subtitle);

        btnVideoTracks = findViewById(R.id.exo_video_tracks);
        btnAudioTracks = findViewById(R.id.exo_audio_tracks);

        llEpg = findViewById(R.id.ll_player_epg);
        epgTitle = findViewById(R.id.tv_epg_title);
        epgTime = findViewById(R.id.tv_epg_time);

        previous = findViewById(R.id.iv_previous);
        next = findViewById(R.id.iv_next);

        bandwidthMeter = new DefaultBandwidthMeter.Builder(this).build();
        mediaDataSourceFactory = buildDataSourceFactory(true);
        mAudioManager = (AudioManager) getSystemService(Context.AUDIO_SERVICE);

        // Set default cookie manager if not already set
        if (CookieHandler.getDefault() != DEFAULT_COOKIE_MANAGER) {
            CookieHandler.setDefault(DEFAULT_COOKIE_MANAGER);
        }

        updateButtonVisibility(false, false, false);
        setUpPlayer();
        setUpPlayerControls();
        initPlayerLockController();

        runOnUiThread(this::setMediaSource);

        if (isTvBox){
            findViewById(R.id.iv_back_player).setVisibility(View.GONE);
            AppCompatDelegate.setDefaultNightMode(AppCompatDelegate.MODE_NIGHT_YES);
        }

        setupBackPressHandler();
    }

    private void setupBackPressHandler() {
        OnBackPressedCallback callback = new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                if (isVisibleDialogInfo || isVisibleDialogList){
                    closeDialog();
                    return;
                }
                if (isPlayerLocked) {
                    setPlayerLocked(false);
                    return;
                }
                Callback.setIsOrientationPlayer(false);
                finish();
            }
        };
        getOnBackPressedDispatcher().addCallback(this, callback);
    }

    private void setUpPlayerControls() {
        btnTryAgain.setOnClickListener(v -> {
            btnTryAgain.setVisibility(View.GONE);
            previous.setVisibility(View.VISIBLE);
            next.setVisibility(View.VISIBLE);
            loadingProgressBar.setVisibility(View.VISIBLE);
            setMediaSource();
        });

        btnPlay.setOnClickListener(v -> {
            exoPlayer.setPlayWhenReady(!exoPlayer.getPlayWhenReady());
            btnPlay.setImageResource(exoPlayer.getPlayWhenReady() ? R.drawable.ic_pause : R.drawable.ic_play);
        });
        if (isTvBox){
            btnPlay.requestFocus();
        }

        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
            btnFav.setVisibility(View.GONE);
        }
        btnFav.setOnClickListener(v -> setFav());
        previous.setOnClickListener(v -> previous());
        next.setOnClickListener(v -> next());
        findViewById(R.id.ll_multiple_screen).setOnClickListener(v -> openMultipleScreenActivity());

        setBatteryInfo();
        findViewById(R.id.ll_aspect_ratio).setOnClickListener(v -> setResize());
        findViewById(R.id.exo_media_info).setOnClickListener(v -> visibleDialogInfo(true));
        findViewById(R.id.iv_back_player).setOnClickListener(v -> {
            Callback.setIsOrientationPlayer(false);
            finish();
        });
        ImageView pipButton  = findViewById(R.id.exo_pip);
        if (isPipSupported()) {
            pipButton.setVisibility(View.VISIBLE);
            pipButton.setOnClickListener(v -> enterPipMode());
        } else {
            pipButton.setVisibility(View.GONE);
        }

        ImageView controllerClose = findViewById(R.id.exo_controller_close);
        if (!isTvBox){
            controllerClose.setOnClickListener(v -> playerView.hideController());
        } else {
            if (controllerClose.getVisibility() != View.GONE){
                controllerClose.setVisibility(View.GONE);
            }
        }

        setUpTracks();
    }

    private void initPlayerLockController() {
        lockOverlay = findViewById(R.id.player_lock_overlay);
        lockOverlayContent = findViewById(R.id.player_lock_overlay_content);
        lockToggleButton = findViewById(R.id.exo_lock);
        unlockPlayerButton = findViewById(R.id.btn_unlock_player);

        if (isTvBox) {
            if (lockToggleButton != null) {
                lockToggleButton.setVisibility(View.GONE);
            }
            if (lockOverlay != null) {
                lockOverlay.setVisibility(View.GONE);
            }
            return;
        }

        if (lockOverlay != null) {
            lockOverlay.setOnClickListener(v -> showLockOverlayHint());
        }
        if (lockToggleButton != null) {
            lockToggleButton.setOnClickListener(v -> togglePlayerLock());
        }
        if (unlockPlayerButton != null) {
            unlockPlayerButton.setOnClickListener(v -> playUnlockAnimation(() -> setPlayerLocked(false)));
        }
        setPlayerLocked(false);
    }

    private void togglePlayerLock() {
        setPlayerLocked(!isPlayerLocked);
    }

    private void setPlayerLocked(boolean locked) {
        isPlayerLocked = locked;

        if (lockToggleButton != null) {
            lockToggleButton.setImageResource(locked ? R.drawable.ic_lock : R.drawable.ic_unlock);
            lockToggleButton.setContentDescription(getString(locked ? R.string.unlock : R.string.lock_player));
        }

        if (lockOverlay != null) {
            if (locked) {
                lockOverlay.setVisibility(View.VISIBLE);
            } else {
                lockOverlay.setVisibility(View.GONE);
                lockOverlay.removeCallbacks(hideLockOverlayRunnable);
            }
        }

        if (playerView != null) {
            playerView.setUseController(!locked);
            playerView.setControllerAutoShow(!locked);
            if (locked) {
                playerView.hideController();
            } else {
                playerView.showController();
            }
        }
        if (locked) {
            showLockOverlayHint();
        } else {
            hideLockOverlayHint();
        }
    }

    private void updateLiveHeaderInfo() {
        if (liveResolutionView != null) {
            String resolution = "--";
            if (exoPlayer != null && exoPlayer.getVideoFormat() != null) {
                Format videoFormat = exoPlayer.getVideoFormat();
                int height = videoFormat.height;
                if (height >= 2160) {
                    resolution = "4K";
                } else if (height >= 1440) {
                    resolution = "2K";
                } else if (height >= 1080) {
                    resolution = "FHD";
                } else if (height >= 720) {
                    resolution = "HD";
                } else if (height > 0) {
                    resolution = height + "p";
                }
            }
            liveResolutionView.setText(resolution);
        }
        updateLiveClock();
    }

    private void updateLiveClock() {
        if (liveClockView == null) {
            return;
        }

        boolean use12HourFormat = spHelper != null && spHelper.getIs12Format();

        String pattern = use12HourFormat ? "hh:mm a" : "HH:mm";

        SimpleDateFormat sdf = new SimpleDateFormat(pattern, Locale.getDefault());
        liveClockView.setText(sdf.format(new Date()));
    }

    private void playUnlockAnimation(@NonNull Runnable onEnd) {
        if (lockOverlayContent == null) {
            onEnd.run();
            return;
        }
        lockOverlayContent.animate().cancel();
        lockOverlayContent.setVisibility(View.VISIBLE);
        lockOverlayContent.setAlpha(1f);
        lockOverlayContent.setScaleX(1f);
        lockOverlayContent.setScaleY(1f);
        lockOverlayContent.animate()
                .scaleX(1.1f)
                .scaleY(1.1f)
                .setDuration(140)
                .withEndAction(() -> lockOverlayContent.animate()
                        .alpha(0f)
                        .setDuration(200)
                        .withEndAction(onEnd)
                        .start())
                .start();
    }

    private void showLockOverlayHint() {
        if (lockOverlay == null || lockOverlayContent == null) {
            return;
        }
        lockOverlayContent.animate().cancel();
        lockOverlayContent.setAlpha(1f);
        lockOverlayContent.setScaleX(1f);
        lockOverlayContent.setScaleY(1f);
        lockOverlayContent.setVisibility(View.VISIBLE);
        if (unlockPlayerButton != null) {
            unlockPlayerButton.requestFocus();
        }
        lockOverlay.removeCallbacks(hideLockOverlayRunnable);
        lockOverlay.postDelayed(hideLockOverlayRunnable, LOCK_HINT_TIMEOUT_MS);
    }

    private void hideLockOverlayHint() {
        if (lockOverlay == null || lockOverlayContent == null) {
            return;
        }
        lockOverlay.removeCallbacks(hideLockOverlayRunnable);
        lockOverlayContent.animate().cancel();
        lockOverlayContent.setScaleX(1f);
        lockOverlayContent.setScaleY(1f);
        lockOverlayContent.setAlpha(0f);
        lockOverlayContent.setVisibility(View.INVISIBLE);
        if (unlockPlayerButton != null) {
            unlockPlayerButton.clearFocus();
        }
    }

    private void setUpTracks() {
        btnSubtitle.setOnClickListener(v -> {
            TrackSelectionParameters parameters = exoPlayer.getTrackSelectionParameters();
            boolean isSubtitleEnabled = !parameters.disabledTrackTypes.contains(C.TRACK_TYPE_TEXT);
            if (isSubtitleEnabled) {
                exoPlayer.setTrackSelectionParameters(
                        parameters.buildUpon()
                                .setTrackTypeDisabled(C.TRACK_TYPE_TEXT, true)
                                .build());
                btnSubtitle.setImageResource(R.drawable.ic_closed_captioning);
            } else {
                exoPlayer.setTrackSelectionParameters(
                        parameters.buildUpon()
                                .setTrackTypeDisabled(C.TRACK_TYPE_TEXT, false)
                                .build());
                btnSubtitle.setImageResource(R.drawable.ic_closed_captioning_fill);
            }
        });

        btnVideoTracks.setOnClickListener(v -> showTracksDialog(false));
        btnAudioTracks.setOnClickListener(v -> showTracksDialog(true));

        ImageView streamFormat = findViewById(R.id.exo_stream_format);
        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_STREAM)
                || spHelper.getLoginType().equals(Callback.TAG_LOGIN_ONE_UI)){
            streamFormat.setOnClickListener(v ->
                    startActivity(
                            new Intent(ExoPlayerLiveActivity.this, SettingFormatActivity.class)
                    )
            );
        } else {
            streamFormat.setVisibility(View.GONE);
        }
    }

    private void setUpPlayer() {
        // https://github.com/google/ExoPlayer/issues/8571
        DefaultExtractorsFactory extractorsFactory = PlayerUtils.getDefaultExtractorsFactory();
        DefaultRenderersFactory renderersFactory = PlayerUtils.getDefaultRenderersFactory(this, spHelper.isHardwareDecoding());

        DefaultTrackSelector trackSelector = PlayerUtils.getDefaultTrackSelector(this);

        DefaultLoadControl fastLoadControl = new DefaultLoadControl.Builder()
                .setBufferDurationsMs(40_000, 150_000, 1_000, 3_000)
                .build();

        DefaultMediaSourceFactory mediaSourceFactory = new DefaultMediaSourceFactory(mediaDataSourceFactory, extractorsFactory);

        // Build ExoPlayer instance
        exoPlayer = new ExoPlayer.Builder(this, renderersFactory)
                .setTrackSelector(trackSelector)
                .setLoadControl(fastLoadControl)
                .setMediaSourceFactory(mediaSourceFactory)
                .build();

        // Set audio attributes for the player
        AudioAttributes audioAttributes = new AudioAttributes.Builder()
                .setUsage(C.USAGE_MEDIA)
                .setContentType(C.AUDIO_CONTENT_TYPE_MOVIE)
                .build();
        exoPlayer.setAudioAttributes(audioAttributes, true);

        exoPlayer.setHandleAudioBecomingNoisy(!isTvBox);
        setMediaSession();

        // Attach ExoPlayer to the player view
        playerView = findViewById(R.id.nSoftsPlayerView);
        playerView.setPlayer(exoPlayer);
        playerView.setUseController(true);
        playerView.requestFocus();
        playerView.setControllerHideOnTouch(false);
        playerView.setControllerAutoShow(true);
        playerView.setBrightnessControl(new BrightnessVolumeControl(this));

        // Set controller visibility listener
        playerView.setControllerVisibilityListener((PlayerView.ControllerVisibilityListener) visibility -> {
            controllerVisible = visibility == View.VISIBLE;
            controllerVisibleFully = playerView.isControllerFullyVisible();

            if (isPlayerLocked) {
                return;
            }

            if (isTvBox && visibility == View.VISIBLE) {
                // Because when using dpad controls, focus resets to first item in bottom controls bar
                btnPlay.requestFocus();
            }
        });

        PlayerUtils.setCustomSubtitle(this, playerView);

        // Set player event listeners
        playerListener = new PlayerListener();
        exoPlayer.addListener(playerListener);
    }

    private void enterPipMode() {
        if (!isPipSupported()) {
            Toasty.makeText(this, true, "PiP not supported or Player not ready", Toasty.ERROR);
            return;
        }

        if (!hasPipPermission()) {
            requestPipPermission();
            return;
        }

        if (!isPlayerReady()) {
            Toasty.makeText(this, true, "Player not ready", Toasty.ERROR);
            return;
        }

        if (isPlayerLocked) {
            setPlayerLocked(false);
        }

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O && !isInPictureInPictureMode()) {
            playerView.setControllerAutoShow(false);
            playerView.hideController();

            final Format format = exoPlayer.getVideoFormat();
            if (format != null) {
                // ExoPlayer issue: https://github.com/google/ExoPlayer/issues/8611
                View videoSurfaceView = playerView.getVideoSurfaceView();
                if (videoSurfaceView instanceof SurfaceView surfaceView && format.width > 0 && format.height > 0) {
                    surfaceView.getHolder().setFixedSize(format.width, format.height);
                }
            }

            final Rational rationalLimitWide = new Rational(239, 100);
            final Rational rationalLimitTall = new Rational(100, 239);

            Rational rational = ApplicationUtil.getRational(format);
            if (rational.floatValue() > rationalLimitWide.floatValue()) {
                rational = rationalLimitWide;
            } else if (rational.floatValue() < rationalLimitTall.floatValue()) {
                rational = rationalLimitTall;
            }

            PictureInPictureParams params = new PictureInPictureParams.Builder()
                    .setAspectRatio(rational)
                    .build();

            enterPictureInPictureMode(params);
        }
    }

    private boolean isPlayerReady() {
        return exoPlayer != null && playerView != null;
    }

    private boolean hasPipPermission() {
        if (Build.VERSION.SDK_INT < Build.VERSION_CODES.O) {
            return false;
        }
        AppOpsManager appOpsManager = (AppOpsManager) getSystemService(Context.APP_OPS_SERVICE);
        int mode;
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            // API 29+: use unsafeCheckOpNoThrow (since checkOpNoThrow is deprecated)
            mode = appOpsManager.unsafeCheckOpNoThrow(
                    AppOpsManager.OPSTR_PICTURE_IN_PICTURE,
                    android.os.Process.myUid(),
                    getPackageName());
        } else {
            // Older API levels (O, P) - checkOpNoThrow isn't deprecated yet
            mode = appOpsManager.checkOpNoThrow(
                    AppOpsManager.OPSTR_PICTURE_IN_PICTURE,
                    android.os.Process.myUid(),
                    getPackageName());
        }

        return mode == AppOpsManager.MODE_ALLOWED;
    }

    private void requestPipPermission() {
        final Intent intent = new Intent("android.settings.PICTURE_IN_PICTURE_SETTINGS",
                Uri.fromParts("package", getPackageName(), null));
        if (intent.resolveActivity(getPackageManager()) != null) {
            startActivity(intent);
        }
    }

    @Override
    public void onPictureInPictureModeChanged(boolean isInPictureInPictureMode, @NonNull Configuration newConfig) {
        super.onPictureInPictureModeChanged(isInPictureInPictureMode, newConfig);
        isInPicInPicMode = isInPictureInPictureMode;
        if (playerView == null) {
            return;
        }
        if (isInPictureInPictureMode) {
            playerView.setUseController(false);
            playerView.setControllerAutoShow(false);
            playerView.hideController();
        } else {
            playerView.setUseController(true);
            playerView.setControllerAutoShow(true);
            playerView.showController();
        }
    }

    @Override
    protected void onUserLeaveHint() {
        super.onUserLeaveHint();
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O && isPipSupported() && exoPlayer != null && exoPlayer.isPlaying()) {
            enterPipMode();
        }
    }

    private boolean isPipSupported() {
        return Build.VERSION.SDK_INT >= Build.VERSION_CODES.O && getPackageManager().hasSystemFeature(PackageManager.FEATURE_PICTURE_IN_PICTURE);
    }

    private void openMultipleScreenActivity() {
        Intent intent = new Intent(ExoPlayerLiveActivity.this, MultipleScreenActivity.class);
        intent.putExtra("is_player", true);
        intent.putExtra("stream_id", Callback.getArrayListLive().get(Callback.getPlayPosLive()).getStreamID());
        startActivity(intent);
        finish();
    }

    private void setFav() {
        if (Callback.getArrayListLive().isEmpty()){
            return;
        }
        if (dbHelper.checkLive(DBHelper.TABLE_FAV_LIVE, Callback.getArrayListLive().get(Callback.getPlayPosLive()).getStreamID())){
            dbHelper.removeLive(DBHelper.TABLE_FAV_LIVE, Callback.getArrayListLive().get(Callback.getPlayPosLive()).getStreamID());
            btnFav.setImageResource(R.drawable.ic_favorite_border);
            Toast.makeText(ExoPlayerLiveActivity.this, getString(R.string.fav_remove_success), Toast.LENGTH_SHORT).show();
        } else {
            dbHelper.addToLive(DBHelper.TABLE_FAV_LIVE, Callback.getArrayListLive().get(Callback.getPlayPosLive()), 0);
            btnFav.setImageResource(R.drawable.ic_favorite);
            Toast.makeText(ExoPlayerLiveActivity.this, getString(R.string.fav_success), Toast.LENGTH_SHORT).show();
        }
    }

    private void setMediaSession() {
        if (mediaSession != null) {
            mediaSession.release();
        }
        if (exoPlayer.canAdvertiseSession()) {
            try {
                mediaSession = new MediaSession.Builder(this, exoPlayer).build();
            } catch (Exception e) {
                ApplicationUtil.log(TAG, "Failed to create MediaSession", e);
            }
        }
    }

    private void setBatteryInfo() {
        ImageView batteryInfo = findViewById(R.id.exo_battery_info);
        if (!isTvBox){
            batteryReceiver = new BroadcastReceiver() {
                @Override
                public void onReceive(Context context, Intent intent) {
                    int status = intent.getIntExtra(BatteryManager.EXTRA_STATUS, -1);
                    int level = intent.getIntExtra(BatteryManager.EXTRA_LEVEL, -1);
                    int scale = intent.getIntExtra(BatteryManager.EXTRA_SCALE, -1);
                    batteryInfo.setImageResource(ApplicationUtil.getBatteryDrawable(status,level,scale));
                }
            };
            IntentFilter filter = new IntentFilter(Intent.ACTION_BATTERY_CHANGED);
            registerReceiver(batteryReceiver, filter);
        } else {
            batteryInfo.setVisibility(View.INVISIBLE);
        }
    }

    private void showTracksDialog(boolean isAudio) {
        if (exoPlayer == null) {
            return;
        }

        if (isAudio){
            AudioTracksUtils.showAudioTracksDialog(this, exoPlayer, (groupIndex, trackIndex) ->
                    AudioTracksUtils.selectAudioTrack(exoPlayer, groupIndex, trackIndex)
            );
        } else {
            VideoTracksUtils.showVideoTracksDialog(this, exoPlayer, selectedIndex ->
                    VideoTracksUtils.selectVideoTrack(exoPlayer, selectedIndex)
            );
        }
    }

    private class PlayerListener implements Player.Listener {

        @Override
        public void onTracksChanged(@NonNull Tracks tracks) {
            Player.Listener.super.onTracksChanged(tracks);
            if (isFinishing()){
                return;
            }

            boolean hasSubtitles = false;
            boolean hasMultipleVideoTracks = false;
            boolean hasMultipleAudioTracks = false;

            for (Tracks.Group group  : tracks.getGroups()) {
                if (!hasSubtitles && group.getType() == C.TRACK_TYPE_TEXT) {
                    Format format = group.getTrackFormat(0);
                    hasSubtitles = format.sampleMimeType != null && format.sampleMimeType.startsWith("text");
                }

                if (!hasMultipleVideoTracks && group.getType() == C.TRACK_TYPE_VIDEO) {
                    hasMultipleVideoTracks = group.length > 1;
                }

                if (!hasMultipleAudioTracks && group.getType() == C.TRACK_TYPE_AUDIO) {
                    hasMultipleAudioTracks = group.length > 1;
                }

                if (hasSubtitles && hasMultipleVideoTracks && hasMultipleAudioTracks) {
                    break;
                }
            }

            updateButtonVisibility(hasSubtitles, hasMultipleVideoTracks, hasMultipleAudioTracks);
            updateLiveHeaderInfo();
        }

        @Override
        public void onVideoSizeChanged(@NonNull VideoSize videoSize) {
            Player.Listener.super.onVideoSizeChanged(videoSize);
            if (isFinishing()){
                return;
            }
            updateLiveHeaderInfo();
        }

        @Override
        public void onAudioSessionIdChanged(int audioSessionId) {
            Player.Listener.super.onAudioSessionIdChanged(audioSessionId);
            if (isFinishing()){
                return;
            }
            setLoudnessEnhancer(audioSessionId);
        }

        @Override
        public void onIsPlayingChanged(boolean isPlaying) {
            Player.Listener.super.onIsPlayingChanged(isPlaying);
            if (isFinishing()){
                return;
            }
            playerView.setKeepScreenOn(isPlaying);
        }

        @Override
        public void onPlaybackStateChanged(int state) {
            Player.Listener.super.onPlaybackStateChanged(state);
            if (isFinishing()){
                return;
            }

            if (state == PlaybackStateEvent.STATE_PLAYING) {
                btnPlay.setImageResource(R.drawable.ic_pause);
                loadingProgressBar.setVisibility(View.GONE);
                playback = 1;
                startTimer();
            } else if (state == Player.STATE_BUFFERING) {
                loadingProgressBar.setVisibility(View.VISIBLE);
            }
        }
        @Override
        public void onPlayerError(@NonNull PlaybackException error) {
            Player.Listener.super.onPlayerError(error);
            if (isFinishing()){
                return;
            }
            if (playback < 5){
                playback = playback + 1;
                Toast.makeText(ExoPlayerLiveActivity.this,"Playback error - "+ playback + "/5 ", Toast.LENGTH_SHORT).show();
                new Handler(Looper.getMainLooper()).postDelayed(() -> {
                    if (isFinishing()){
                        return;
                    }
                    setMediaSource();
                }, 600);
            } else {
                playback = 1;
                exoPlayer.stop();
                btnTryAgain.setVisibility(View.VISIBLE);
                if (previous.getVisibility() == View.VISIBLE){
                    previous.setVisibility(View.INVISIBLE);
                }
                if (next.getVisibility() == View.VISIBLE){
                    next.setVisibility(View.INVISIBLE);
                }
                btnPlay.setImageResource(R.drawable.ic_play);
                btnPlay.setVisibility(View.GONE);
                loadingProgressBar.setVisibility(View.GONE);
                Toast.makeText(ExoPlayerLiveActivity.this,"Failed : " + error.getErrorCodeName(), Toast.LENGTH_SHORT).show();
            }
        }
    }

    public void updateButtonVisibility(boolean hasSubtitles,
                                       boolean hasMultipleVideoTracks,
                                       boolean hasMultipleAudioTracks) {
        if (isFinishing()){
            return;
        }

        // Update subtitle button
        if (hasSubtitles){
            btnSubtitle.setImageResource(R.drawable.ic_closed_captioning);
        }
        btnSubtitle.setVisibility(hasSubtitles ? View.VISIBLE : View.GONE);

        // Update video and audio tracks buttons
        btnVideoTracks.setVisibility(hasMultipleVideoTracks ? View.VISIBLE : View.GONE);
        btnAudioTracks.setVisibility(hasMultipleAudioTracks ? View.VISIBLE : View.GONE);
    }

    private void setLoudnessEnhancer(int audioSessionId) {
        try {
            if (audioSessionId <= 0) {
                ApplicationUtil.log(TAG, "Invalid audioSessionId: " + audioSessionId);
                return;
            }

            if (loudnessEnhancer != null) {
                loudnessEnhancer.release();
            }
            loudnessEnhancer = new LoudnessEnhancer(audioSessionId);
            notifyAudioSessionUpdate(true);
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "Failed to create loudness enhancer", e);
            loudnessEnhancer = null;  // Ensure it is null in case of failure
        }
    }

    public void startTimer() {
        if (isFinishing() || isInPicInPicMode || isTvBox || isVisibleDialogInfo){
            return;
        }
        if (countDownTimer == null && Boolean.TRUE.equals(Callback.getRewardAdMovie())){
            countDownTimer = new CountDownTimer(timeLeftInMillis, 1000) {

                @Override
                public void onTick(long millisUntilFinished) {
                    timeLeftInMillis = millisUntilFinished;
                }

                @Override
                public void onFinish() {
                    if (isFinishing() || isInPicInPicMode || isVisibleDialogInfo){
                        return;
                    }
                    boolean isPlaying = exoPlayer != null && exoPlayer.isPlaying();
                    helper.showRewardAds(Callback.getRewardAdMovie(),isPlaying, playWhenReady1 -> {
                    });
                }
            }.start();
        }
    }

    void notifyAudioSessionUpdate(final boolean active) {
        final Intent intent = new Intent(active
                ? AudioEffect.ACTION_OPEN_AUDIO_EFFECT_CONTROL_SESSION
                : AudioEffect.ACTION_CLOSE_AUDIO_EFFECT_CONTROL_SESSION
        );
        intent.putExtra(AudioEffect.EXTRA_AUDIO_SESSION, exoPlayer.getAudioSessionId());
        intent.putExtra(AudioEffect.EXTRA_PACKAGE_NAME, getPackageName());
        if (active) {
            intent.putExtra(AudioEffect.EXTRA_CONTENT_TYPE, AudioEffect.CONTENT_TYPE_MOVIE);
        }
        try {
            sendBroadcast(intent);
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "notifyAudioSessionUpdate: ", e);
        }
    }

    private void handlePreviousNextBtn() {
        int currentPos = Callback.getPlayPosLive();
        int lastPos = Callback.getArrayListLive().size() - 1;
        previous.setVisibility(currentPos > 0 ? View.VISIBLE : View.INVISIBLE);
        next.setVisibility(currentPos < lastPos ? View.VISIBLE : View.INVISIBLE);
    }

    public void next() {
        if (validatePlaybackConditions()){
            return;
        }
        if (Callback.getPlayPosLive() < (Callback.getArrayListLive().size() - 1)) {
            Callback.setPlayPosLive(Callback.getPlayPosLive() + 1);
        } else {
            Callback.setPlayPosLive(0);
        }

        recreate();
    }

    public void previous() {
        if (validatePlaybackConditions()){
            return;
        }
        if (Callback.getPlayPosLive() > 0) {
            Callback.setPlayPosLive(Callback.getPlayPosLive() - 1);
        } else {
            Callback.setPlayPosLive(Callback.getArrayListLive().size() - 1);
        }

        recreate();
    }

    private void setMediaSource() {
        if (validatePlaybackConditions()){
            return;
        }

        // Ensure player is not null
        if (exoPlayer == null) {
            ApplicationUtil.log(TAG, "ExoPlayer is null, cannot set media source");
            return;
        }

        // Ensure live list and play position are valid
        if (Callback.getArrayListLive().isEmpty()) {
            ApplicationUtil.log(TAG, "Live list is null or empty");
            return;
        }
        if (Callback.getPlayPosLive() < 0 || Callback.getPlayPosLive() >= Callback.getArrayListLive().size()) {
            ApplicationUtil.log(TAG, "Invalid play position: " + Callback.getPlayPosLive());
            return;
        }

        findViewById(R.id.ll_channels_list).setOnClickListener(view -> visibleDialogList(true));

        if (btnTryAgain.getVisibility() == View.VISIBLE){
            btnTryAgain.setVisibility(View.GONE);
            previous.setVisibility(View.VISIBLE);
            next.setVisibility(View.VISIBLE);
        }

        playerTitle.setText(Callback.getArrayListLive().get(Callback.getPlayPosLive()).getName());
        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_ONE_UI) || spHelper.getLoginType().equals(Callback.TAG_LOGIN_STREAM)){
            btnFav.setImageResource(dbHelper.checkLive(DBHelper.TABLE_FAV_LIVE,
                    Callback.getArrayListLive().get(Callback.getPlayPosLive()).getStreamID()) ? R.drawable.ic_favorite : R.drawable.ic_favorite_border);
        }

        String url = getChannelUrl();
        if (url == null) {
            ApplicationUtil.log(TAG, "Channel URL is null");
            return;
        }

        Uri uri = Uri.parse(url);
        MediaSource mediaSource = buildMediaSource(uri);
        exoPlayer.setMediaSource(mediaSource);

        setLoudnessEnhancer(exoPlayer.getAudioSessionId());

        exoPlayer.prepare();
        exoPlayer.setPlayWhenReady(true);
        btnPlay.setImageResource(R.drawable.ic_pause);
        btnPlay.setVisibility(View.VISIBLE);

        handlePreviousNextBtn();

        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_ONE_UI) || spHelper.getLoginType().equals(Callback.TAG_LOGIN_STREAM)){
            try {
                dbHelper.addToLive(DBHelper.TABLE_RECENT_LIVE, Callback.getArrayListLive().get(Callback.getPlayPosLive()), spHelper.getLiveLimit());
            } catch (Exception e) {
                ApplicationUtil.log(TAG, "setMediaSource add to live: ", e);
            }
        }
    }

    private void visibleDialogInfo(boolean visible) {
        if (visible){
            if (exoPlayer == null) {
                return;
            }
            boolean isPlaying = exoPlayer.getPlayWhenReady();
            boolean hasVideo = exoPlayer.getVideoFormat() != null;
            if (isPlaying && hasVideo) {
                playerView.setUseController(false);
                playerView.setControllerAutoShow(false);
                playerView.hideController();
                playerView.setPadding(100,100,100,100);
                isVisibleDialogInfo = true;
                findViewById(R.id.ll_media).setVisibility(View.VISIBLE);
                findViewById(R.id.vw_close).setVisibility(View.VISIBLE);
                findViewById(R.id.vw_close).setOnClickListener(v -> visibleDialogInfo(false));
                findViewById(R.id.back_player_info).setOnClickListener(v -> closeDialog());
                if (isTvBox){
                    findViewById(R.id.back_player_info).requestFocus();
                }

                String infoVideo = ApplicationUtil.getInfoVideo(exoPlayer, true);
                TextView mediaVideo = findViewById(R.id.text_info_video);
                mediaVideo.setText(infoVideo);

                String infoAudio = ApplicationUtil.getInfoAudio(exoPlayer);
                TextView mediaAudio = findViewById(R.id.text_info_audio);
                mediaAudio.setText(infoAudio);
            } else {
                Toasty.makeText(this,true, getString(R.string.please_wait_a_minute), Toasty.ERROR);
            }
        } else {
            playerView.setUseController(true);
            playerView.setControllerAutoShow(true);
            playerView.showController();
            playerView.setPadding(0,0,0,0);
            isVisibleDialogInfo = false;
            findViewById(R.id.vw_close).setVisibility(View.GONE);
            findViewById(R.id.ll_media).setVisibility(View.GONE);
        }
    }

    private void visibleDialogList(boolean visible) {
        if (visible){
            playerView.setUseController(false);
            playerView.setControllerAutoShow(false);
            playerView.hideController();
            playerView.setPadding(100,100,100,100);
            isVisibleDialogList = true;
            findViewById(R.id.vw_close).setVisibility(View.VISIBLE);
            findViewById(R.id.vw_close).setOnClickListener(v -> closeDialog());
            RecyclerView rv = findViewById(R.id.rv_dialog);
            LinearLayoutManager manager = new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false);
            rv.setLayoutManager(manager);
            rv.setItemAnimator(new DefaultItemAnimator());
            AdapterChannelPlayer adapter = new AdapterChannelPlayer(this, Callback.getArrayListLive(), (item, position) -> {
                Callback.setPlayPosLive(position);
                setMediaSource();
            });
            rv.setAdapter(adapter);
            rv.scrollToPosition(Callback.getPlayPosLive());
            adapter.select(Callback.getPlayPosLive());
            rv.setVisibility(View.VISIBLE);
            if (isTvBox){
                rv.requestFocus();
            }
        } else {
            playerView.setUseController(true);
            playerView.setControllerAutoShow(true);
            playerView.showController();
            playerView.setPadding(0,0,0,0);
            isVisibleDialogList = false;
            findViewById(R.id.vw_close).setVisibility(View.GONE);
            findViewById(R.id.rv_dialog).setVisibility(View.GONE);
        }
    }

    private void closeDialog() {
        if (isVisibleDialogList){
            visibleDialogList(false);
        } else if (isVisibleDialogInfo){
            visibleDialogInfo(false);
        }
    }

    private boolean validatePlaybackConditions() {
        if (Callback.getArrayListLive().isEmpty()) {
            Toasty.makeText(ExoPlayerLiveActivity.this,true, getString(R.string.err_no_data_found), Toasty.ERROR);
            return true;
        }
        if (!NetworkUtils.isConnected(this)) {
            Toasty.makeText(ExoPlayerLiveActivity.this,true, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            return true;
        }
        return false;
    }

    private String getChannelUrl() {
        String channelUrl;
        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)){
            channelUrl = Callback.getArrayListLive().get(Callback.getPlayPosLive()).getStreamID();
            llEpg.setVisibility(View.GONE);
        } else {
            String format = PlayerAPI.FORMAT_M3U8;
            if (spHelper.getLiveFormat() == 1){
                format = PlayerAPI.FORMAT_TS;
            }

            channelUrl = PlayerAPI.getLiveURL(
                    spHelper.getIsXuiUser(),spHelper.getServerURL(),
                    spHelper.getUserName(),spHelper.getPassword(),
                    Callback.getArrayListLive().get(Callback.getPlayPosLive()).getStreamID(),
                    format
            );

            if (playback == 0 || playback == 1){
                getEpgData();
            }
        }
        return channelUrl;
    }

    private void getEpgData() {
        if (NetworkUtils.isConnected(this)){
            LoadEpg loadSeriesID = new LoadEpg(this, new EpgListener() {
                @Override
                public void onStart() {
                    llEpg.setVisibility(View.GONE);
                    epgTitle.setText("");
                    epgTime.setText("");
                }

                @SuppressLint("SetTextI18n")
                @Override
                public void onEnd(String success, ArrayList<ItemEpg> epgArrayList) {
                    if (!isFinishing() && !epgArrayList.isEmpty()){
                        epgTitle.setText(EncrypterUtils.decodeBase64(epgArrayList.get(0).getTitle()));
                        epgTime.setText(FormatUtils.getTimestamp(epgArrayList.get(0).getStartTimestamp(),
                                spHelper.getIs12Format()) + " - "
                                + FormatUtils.getTimestamp(epgArrayList.get(0).getStopTimestamp(), spHelper.getIs12Format()));
                        llEpg.setVisibility(View.VISIBLE);
                    }
                }
            }, ApplicationUtil.getAPIRequestID("get_short_epg","stream_id",
                    Callback.getArrayListLive().get(Callback.getPlayPosLive()).getStreamID(), spHelper.getUserName(), spHelper.getPassword()));
            loadSeriesID.execute();
        }
    }

    @SuppressLint("SwitchIntDef")
    @NonNull
    private MediaSource buildMediaSource(Uri uri) {
        int type = Util.inferContentType(uri);
        MediaItem mediaItem = MediaItem.fromUri(uri);
        switch (type) {
            case C.CONTENT_TYPE_SS -> {
                // For SmoothStreaming (SS)
                return new SsMediaSource.Factory(new DefaultSsChunkSource.Factory(mediaDataSourceFactory),
                        buildDataSourceFactory(false)).createMediaSource(mediaItem);
            }
            case C.CONTENT_TYPE_DASH -> {
                // For Dynamic Adaptive Streaming over HTTP (DASH)
                return new DashMediaSource.Factory(new DefaultDashChunkSource.Factory(mediaDataSourceFactory),
                        buildDataSourceFactory(false)).createMediaSource(mediaItem);
            }
            case C.CONTENT_TYPE_HLS -> {
                // For HTTP Live Streaming (HLS)
                return new HlsMediaSource.Factory(mediaDataSourceFactory).createMediaSource(mediaItem);
            }
            case C.CONTENT_TYPE_RTSP -> {
                // For Real-Time Streaming Protocol (RTSP)
                return new RtspMediaSource.Factory().createMediaSource(mediaItem);
            }
            case C.CONTENT_TYPE_OTHER -> {
                // For Progressive Media
                return new ProgressiveMediaSource.Factory(mediaDataSourceFactory).createMediaSource(mediaItem);
            }
            default -> {
                return new ProgressiveMediaSource.Factory(mediaDataSourceFactory).createMediaSource(mediaItem);
            }
        }
    }

    private DataSource.Factory buildDataSourceFactory(boolean useBandwidthMeter) {
        return buildDataSourceFactory(useBandwidthMeter ? bandwidthMeter : null);
    }

    public DataSource.Factory buildDataSourceFactory(DefaultBandwidthMeter bandwidthMeter) {
        HttpDataSource.Factory httpDataSourceFactory = buildHttpDataSourceFactory(bandwidthMeter);
        return new DefaultDataSource.Factory(this, httpDataSourceFactory);
    }

    public HttpDataSource.Factory buildHttpDataSourceFactory(DefaultBandwidthMeter bandwidthMeter) {
        CookieManager cookieManager = new CookieManager();
        cookieManager.setCookiePolicy(CookiePolicy.ACCEPT_ORIGINAL_SERVER);
        CookieHandler.setDefault(cookieManager);

        return new DefaultHttpDataSource.Factory()
                .setUserAgent(spHelper.getAgentName().isEmpty()
                        ? Util.getUserAgent(ExoPlayerLiveActivity.this, "ExoPlayerDemo")
                        : spHelper.getAgentName())
                .setTransferListener(bandwidthMeter)
                .setAllowCrossProtocolRedirects(true)
                .setKeepPostFor302Redirects(true);
    }

    private void setResize() {
        if (exoPlayer == null || playerView == null) {
            return;
        }

        switch (resize) {
            case 1:
                playerView.setResizeMode(AspectRatioFrameLayout.RESIZE_MODE_FILL);
                resize = 2;
                break;
            case 2:
                playerView.setResizeMode(AspectRatioFrameLayout.RESIZE_MODE_ZOOM);
                resize = 3;
                break;
            case 3:
                playerView.setResizeMode(AspectRatioFrameLayout.RESIZE_MODE_FIT);
                resize = 1;
                break;
            default:
                resize = 1;
        }

        exoPlayer.setVideoScalingMode(C.VIDEO_SCALING_MODE_DEFAULT);
        playerView.showController();
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_player_live;
    }

    @Override
    public void onStop() {
        super.onStop();
        playWhenReady(false);
        liveClockHandler.removeCallbacks(liveClockRunnable);
    }

    @Override
    protected void onStart() {
        super.onStart();
        liveClockHandler.removeCallbacks(liveClockRunnable);
        liveClockHandler.post(liveClockRunnable);
    }

    @Override
    public void onPause() {
        super.onPause();
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
            if (isInPictureInPictureMode()) {
                if (exoPlayer != null && exoPlayer.isPlaying()) {
                    exoPlayer.pause();
                }
            } else {
                playWhenReady(false);
            }
        } else {
            playWhenReady(false);
        }
    }

    @Override
    public void onResume() {
        super.onResume();
        playWhenReady(true);
    }

    @Override
    protected void onRestart() {
        super.onRestart();
        playWhenReady(true);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        release();
    }

    private void release() {
        if (countDownTimer != null) {
            countDownTimer.cancel();
        }

        if (batteryReceiver != null){
            unregisterReceiver(batteryReceiver);
            batteryReceiver = null;
        }

        if (exoPlayer != null) {
            notifyAudioSessionUpdate(false);
            exoPlayer.removeListener(playerListener);
            exoPlayer.release();
            exoPlayer = null;
        }

        if (mediaSession != null) {
            mediaSession.release();
            mediaSession = null;
        }

        if (loudnessEnhancer != null) {
            loudnessEnhancer.release();
            loudnessEnhancer = null;
        }
    }

    private void playWhenReady(boolean setPlayWhenReady) {
        try {
            if (exoPlayer == null) {
                return;
            }
            exoPlayer.setPlayWhenReady(setPlayWhenReady);
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "Failed to set play when ready", e);
        }
    }

    // onKeyUp -----------------------------------------------------------
    @Override
    public boolean onKeyUp(int keyCode, KeyEvent event) {
        if (playerView  == null) {
            return super.onKeyUp(keyCode, event);
        }
        if (keyCode == KeyEvent.KEYCODE_BACK || keyCode ==  KeyEvent.KEYCODE_VOLUME_DOWN){
            playerView.postDelayed(playerView.textClearRunnable, CustomPlayerView.MESSAGE_TIMEOUT_KEY);
        }
        return super.onKeyUp(keyCode, event);
    }

    // onKeyDown --------------------------------------------------------
    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (exoPlayer == null) {
            return super.onKeyDown(keyCode, event); // Handle null exoPlayer early
        }

        if (isPlayerLocked) {
            if (keyCode == KeyEvent.KEYCODE_BACK) {
                setPlayerLocked(false);
                return true;
            }
            if (keyCode == KeyEvent.KEYCODE_VOLUME_UP || keyCode == KeyEvent.KEYCODE_VOLUME_DOWN) {
                adjustVolume(keyCode == KeyEvent.KEYCODE_VOLUME_UP, event.getRepeatCount() == 0);
                return true;
            }
            return true;
        }

        switch (keyCode) {
            case KeyEvent.KEYCODE_MEDIA_NEXT -> next();
            case KeyEvent.KEYCODE_MEDIA_PREVIOUS -> previous();
            case KeyEvent.KEYCODE_MEDIA_PLAY, KeyEvent.KEYCODE_MEDIA_PAUSE,
                 KeyEvent.KEYCODE_BUTTON_SELECT -> {
                handlePlayPauseToggle(keyCode);
                return true;
            }
            case KeyEvent.KEYCODE_MEDIA_PLAY_PAUSE, KeyEvent.KEYCODE_HEADSETHOOK -> {
                handlePlayPause();
                return true;
            }
            case KeyEvent.KEYCODE_VOLUME_UP, KeyEvent.KEYCODE_VOLUME_DOWN -> {
                adjustVolume(keyCode == KeyEvent.KEYCODE_VOLUME_UP, event.getRepeatCount() == 0);
                return true;
            }
            case KeyEvent.KEYCODE_BUTTON_START, KeyEvent.KEYCODE_BUTTON_A, KeyEvent.KEYCODE_ENTER,
                 KeyEvent.KEYCODE_DPAD_CENTER, KeyEvent.KEYCODE_NUMPAD_ENTER,
                 KeyEvent.KEYCODE_SPACE -> {
                handlePlayPauseOnControllerVisibility();
                return true;
            }
            case KeyEvent.KEYCODE_BACK -> {
                handleBackButton();
                return true;
            }
            case KeyEvent.KEYCODE_UNKNOWN -> {
                return super.onKeyDown(keyCode, event);
            }
            default -> {
                ApplicationUtil.log(TAG, "Unhandled key event: " + keyCode);
                return super.onKeyDown(keyCode, event);
            }
        }
        return super.onKeyDown(keyCode, event);
    }

    private void handlePlayPauseToggle(int keyCode) {
        switch (keyCode) {
            case KeyEvent.KEYCODE_MEDIA_PAUSE:
                exoPlayer.pause();
                break;
            case KeyEvent.KEYCODE_MEDIA_PLAY:
                exoPlayer.play();
                break;
            default:
                handlePlayPause();
                break;
        }
    }

    private void handlePlayPause() {
        if (exoPlayer.isPlaying()) {
            exoPlayer.pause();
        } else {
            exoPlayer.play();
        }
    }

    private void handlePlayPauseOnControllerVisibility() {
        if (!controllerVisibleFully) {
            handlePlayPause();
        }
    }

    private void handleBackButton() {
        if (isPlayerLocked) {
            setPlayerLocked(false);
            return;
        }
        if (isVisibleDialogInfo || isVisibleDialogList){
            closeDialog();
            return;
        }
        if (!DeviceUtils.isTvBox(this) ) {
            finish();
            return;
        }
        if (exoPlayer == null || !exoPlayer.isPlaying()) {
            finish();
            return;
        }
        if (controllerVisible){
            playerView.hideController();
            controllerVisible = false;
        } else {
            finish();
        }
    }

    private void adjustVolume(final boolean raise, boolean canBoost) {
        playerView.removeCallbacks(playerView.textClearRunnable);

        final int currentVolume = VolumeHelper.getVolume(this, false, mAudioManager);
        final int maxVolume = VolumeHelper.getVolume(this, true, mAudioManager);
        boolean volumeActive = currentVolume != 0;

        if (currentVolume != maxVolume) boostLevel = 0;

        if (loudnessEnhancer == null)
            canBoost = false;

        if (currentVolume != maxVolume || (boostLevel == 0 && !raise)) {
            VolumeHelper.handleVolumeStep(
                    ExoPlayerLiveActivity.this, mAudioManager, playerView, raise, currentVolume
            );
        } else {
            VolumeHelper.handleBoost(
                    playerView, loudnessEnhancer, boostLevel, raise, canBoost, maxVolume
            );
        }

        playerView.setIconVolume(volumeActive);
        playerView.setHighlight(boostLevel > 0);
        playerView.postDelayed(playerView.textClearRunnable, CustomPlayerView.MESSAGE_TIMEOUT_KEY);
    }
}