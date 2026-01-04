package nemosofts.streambox.activity;

import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.media.metrics.PlaybackStateEvent;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.KeyEvent;
import android.view.View;
import android.view.WindowManager;
import android.view.accessibility.CaptioningManager;
import android.widget.ImageView;
import android.widget.ProgressBar;
import android.widget.Toast;

import androidx.activity.EdgeToEdge;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.media3.common.C;
import androidx.media3.common.MediaItem;
import androidx.media3.common.PlaybackException;
import androidx.media3.common.Player;
import androidx.media3.common.util.UnstableApi;
import androidx.media3.common.util.Util;
import androidx.media3.datasource.DataSource;
import androidx.media3.datasource.DefaultDataSource;
import androidx.media3.datasource.DefaultHttpDataSource;
import androidx.media3.datasource.HttpDataSource;
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
import androidx.media3.exoplayer.trackselection.TrackSelector;
import androidx.media3.exoplayer.upstream.DefaultBandwidthMeter;
import androidx.media3.extractor.DefaultExtractorsFactory;
import androidx.media3.ui.AspectRatioFrameLayout;
import androidx.media3.ui.PlayerView;
import androidx.nemosofts.AppCompatActivity;

import androidx.nemosofts.utils.DeviceUtils;
import androidx.nemosofts.utils.PlayerAPI;

import java.net.CookieHandler;
import java.net.CookieManager;
import java.net.CookiePolicy;
import java.util.Locale;

import nemosofts.streambox.R;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.utils.ApplicationUtil;
import nemosofts.streambox.utils.IfSupported;
import nemosofts.streambox.utils.helper.SPHelper;
import nemosofts.streambox.utils.helper.ThemeHelper;
import nemosofts.streambox.utils.player.PlayerUtils;

@UnstableApi
public class MultipleScreenActivity extends AppCompatActivity {

    private enum PlayerNumber { ONE, TWO, THREE, FOUR }

    private static final String TAG = "MultipleScreenActivity";
    private static final int MAX_RETRIES = 5;
    private static final String TAG_PLAYBACK_ERROR = "Playback error - ";

    private boolean isTvBox;
    private SPHelper spHelper;
    private String streamID = "0";
    private DefaultBandwidthMeter bandwidthMeter;
    private DataSource.Factory mediaDataSourceFactory;
    private boolean isPlayer = false;

    private ExoPlayer exoPlayerOne;
    private ExoPlayer exoPlayerTwo;
    private ExoPlayer exoPlayerThree;
    private ExoPlayer exoPlayerFour;

    private ImageView addOne;
    private ImageView addTwo;
    private ImageView addThree;
    private ImageView addFour;

    private ImageView volumeOne;
    private ImageView volumeTwo;
    private ImageView volumeThree;
    private ImageView volumeFour;

    private ProgressBar pbOne;
    private ProgressBar pbTwo;
    private ProgressBar pbThree;
    private ProgressBar pbFour;

    private int playbackOne = 0;
    private int playbackTwo = 0;
    private int playbackThree = 0;
    private int playbackFour = 0;

    private String urlOne = "";
    private String urlTwo = "";
    private String urlThree = "";
    private String urlFour = "";

    private static final String TAG_STREAM_ID = "stream_id";

    DefaultExtractorsFactory extractorsFactory;
    DefaultRenderersFactory renderersFactory;

    private static final CookieManager DEFAULT_COOKIE_MANAGER;
    static {
        DEFAULT_COOKIE_MANAGER = new CookieManager();
        DEFAULT_COOKIE_MANAGER.setCookiePolicy(CookiePolicy.ACCEPT_ORIGINAL_SERVER);
    }

    @Override
    public void onCreate(Bundle savedInstanceState) {
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        getWindow().addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
        super.onCreate(savedInstanceState);
        EdgeToEdge.enable(this);
        ViewCompat.setOnApplyWindowInsetsListener(findViewById(R.id.theme_bg), (v, insets) -> {
            Insets systemInsets = insets.getInsets(WindowInsetsCompat.Type.systemBars());
            v.setPadding(systemInsets.left, systemInsets.top, systemInsets.right, systemInsets.bottom);
            return insets;
        });

        initializeUI();
        initializeViews();

        setupScreen();
    }

    private void initializeUI() {
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);

        isTvBox = DeviceUtils.isTvBox(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ThemeHelper.getThemeBackgroundRes(this));

        spHelper = new SPHelper(this);

        Intent intent = getIntent();
        isPlayer = intent.getBooleanExtra("is_player", false);
        if (isPlayer){
            streamID = intent.getStringExtra(TAG_STREAM_ID);
        }

        bandwidthMeter = new DefaultBandwidthMeter.Builder(this).build();
        mediaDataSourceFactory = buildDataSourceFactory(true);
        if (CookieHandler.getDefault() != DEFAULT_COOKIE_MANAGER) {
            CookieHandler.setDefault(DEFAULT_COOKIE_MANAGER);
        }

        // https://github.com/google/ExoPlayer/issues/8571
        extractorsFactory = PlayerUtils.getDefaultExtractorsFactory();
        renderersFactory = PlayerUtils.getDefaultRenderersFactory(this, spHelper.isHardwareDecoding());
    }

    private void initializeViews() {
        addOne = findViewById(R.id.iv_add_btn_one);
        addTwo = findViewById(R.id.iv_add_btn_tow);
        addThree = findViewById(R.id.iv_add_btn_three);
        addFour = findViewById(R.id.iv_add_btn_four);
        volumeOne = findViewById(R.id.iv_volume_one);
        volumeTwo = findViewById(R.id.iv_volume_tow);
        volumeThree = findViewById(R.id.iv_volume_three);
        volumeFour = findViewById(R.id.iv_volume_four);

        pbOne = findViewById(R.id.pb_one);
        pbTwo = findViewById(R.id.pb_tow);
        pbThree = findViewById(R.id.pb_three);
        pbFour = findViewById(R.id.pb_four);
    }

    private void setupScreen() {
        if (isPlayer){
            setScreen(spHelper.getScreen());
            setPlayerOne(getChannelUrl(streamID));
        } else {
            if (Boolean.TRUE.equals(spHelper.getIsScreen())){
                DialogUtil.screenDialog(MultipleScreenActivity.this, new DialogUtil.ScreenDialogListener() {
                    @Override
                    public void onSubmit(int screen) {
                        setScreen(screen);
                    }

                    @Override
                    public void onCancel() {
                        // Handle cancel if needed
                    }
                });
            } else {
                setScreen(spHelper.getScreen());
            }
        }
        setOnClickListeners();
    }

    private void setOnClickListeners(){
        addOne.setOnClickListener(v -> openFilterActivity(PlayerNumber.ONE));
        addTwo.setOnClickListener(v -> openFilterActivity(PlayerNumber.TWO));
        addThree.setOnClickListener(v -> openFilterActivity(PlayerNumber.THREE));
        addFour.setOnClickListener(v -> openFilterActivity(PlayerNumber.FOUR));

        findViewById(R.id.vw_player_one).setOnClickListener(v -> setVolume(PlayerNumber.ONE));
        findViewById(R.id.vw_player_tow).setOnClickListener(v -> setVolume(PlayerNumber.TWO));
        findViewById(R.id.vw_player_three).setOnClickListener(v -> setVolume(PlayerNumber.THREE));
        findViewById(R.id.vw_player_four).setOnClickListener(v -> setVolume(PlayerNumber.FOUR));

        findViewById(R.id.vw_player_one).setOnLongClickListener(v -> {
            releasePlayer(exoPlayerOne, pbOne, addOne, R.id.vw_player_one, R.id.player_one, volumeOne);
            return true;
        });
        findViewById(R.id.vw_player_tow).setOnLongClickListener(v -> {
            releasePlayer(exoPlayerTwo, pbTwo, addTwo, R.id.vw_player_tow, R.id.player_tow, volumeTwo);
            return true;
        });
        findViewById(R.id.vw_player_three).setOnLongClickListener(v -> {
            releasePlayer(exoPlayerThree, pbThree, addThree, R.id.vw_player_three, R.id.player_three, volumeThree);
            return true;
        });
        findViewById(R.id.vw_player_four).setOnLongClickListener(v -> {
            releasePlayer(exoPlayerFour, pbFour, addFour, R.id.vw_player_four, R.id.player_four, volumeFour);
            return true;
        });
    }

    private void openFilterActivity(PlayerNumber playerNumber) {
        if (playerNumber == null){
            return;
        }
        Intent intent = spHelper.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)
                ? new Intent(this, FilterPlaylistActivity.class)
                : new Intent(this, FilterActivity.class);

        switch (playerNumber) {
            case ONE:
                filterLauncherPlayerOne.launch(intent);
                break;
            case TWO:
                filterLauncherPlayerTwo.launch(intent);
                break;
            case THREE:
                filterLauncherPlayerThree.launch(intent);
                break;
            case FOUR:
                filterLauncherPlayerFour.launch(intent);
                break;
        }
    }

    private void setVolume(PlayerNumber playerNumber) {
        if (exoPlayerOne != null) exoPlayerOne.setVolume(playerNumber == PlayerNumber.ONE ? 1 : 0);
        if (exoPlayerTwo != null) exoPlayerTwo.setVolume(playerNumber == PlayerNumber.TWO ? 1 : 0);
        if (exoPlayerThree != null) exoPlayerThree.setVolume(playerNumber == PlayerNumber.THREE ? 1 : 0);
        if (exoPlayerFour != null) exoPlayerFour.setVolume(playerNumber == PlayerNumber.FOUR ? 1 : 0);

        setVolumeIcon();
    }

    private void setVolumeIcon() {
        volumeOne.setImageResource(exoPlayerOne != null && exoPlayerOne.getVolume() == 1f ? R.drawable.ic_volume_up : R.drawable.ic_volume_off);
        volumeTwo.setImageResource(exoPlayerTwo != null && exoPlayerTwo.getVolume() == 1f ? R.drawable.ic_volume_up : R.drawable.ic_volume_off);
        volumeThree.setImageResource(exoPlayerThree != null && exoPlayerThree.getVolume() == 1f ? R.drawable.ic_volume_up : R.drawable.ic_volume_off);
        volumeFour.setImageResource(exoPlayerFour != null && exoPlayerFour.getVolume() == 1f ? R.drawable.ic_volume_up : R.drawable.ic_volume_off);
    }

    private void setScreen(int screen) {
        if (screen == 1){
            findViewById(R.id.ll_screen_one_two).setVisibility(View.VISIBLE);
            findViewById(R.id.ll_screen_three_four).setVisibility(View.GONE);
        } else if (screen == 2){
            findViewById(R.id.ll_screen_one_two).setVisibility(View.VISIBLE);
            findViewById(R.id.ll_screen_three_four).setVisibility(View.VISIBLE);
            findViewById(R.id.rl_player_two).setVisibility(View.GONE);
            findViewById(R.id.rl_player_four).setVisibility(View.GONE);
        } else if (screen == 3){
            findViewById(R.id.ll_screen_one_two).setVisibility(View.VISIBLE);
            findViewById(R.id.ll_screen_three_four).setVisibility(View.VISIBLE);
            findViewById(R.id.rl_player_two).setVisibility(View.GONE);
            findViewById(R.id.rl_player_four).setVisibility(View.VISIBLE);
        } else if (screen == 4){
            findViewById(R.id.ll_screen_one_two).setVisibility(View.VISIBLE);
            findViewById(R.id.ll_screen_three_four).setVisibility(View.VISIBLE);
            findViewById(R.id.rl_player_two).setVisibility(View.VISIBLE);
            findViewById(R.id.rl_player_four).setVisibility(View.GONE);
        } else {
            // Default case: show all screens and players
            findViewById(R.id.ll_screen_one_two).setVisibility(View.VISIBLE);
            findViewById(R.id.ll_screen_three_four).setVisibility(View.VISIBLE);
            findViewById(R.id.rl_player_two).setVisibility(View.VISIBLE);
            findViewById(R.id.rl_player_four).setVisibility(View.VISIBLE);
        }

        // Request focus for player one if running on TV Box
        if (DeviceUtils.isTvBox(this)){
            findViewById(R.id.vw_player_one).requestFocus();
        }
    }

    @NonNull
    private String getChannelUrl(String streamId) {
        if (streamId == null) {
            return "";
        }
        if (spHelper.getLoginType().equals(Callback.TAG_LOGIN_PLAYLIST)) {
            return streamId;
        } else {
            String format = PlayerAPI.FORMAT_M3U8;
            if (spHelper.getLiveFormat() == 1){
                format = PlayerAPI.FORMAT_TS;
            }
            return spHelper.getServerURL() + (Boolean.TRUE.equals(spHelper.getIsXuiUser()) ? ""
                    : "live/") + spHelper.getUserName() + "/" + spHelper.getPassword() + "/" + streamId + format;
        }
    }

    private void setPlayerOne(String channelUrl) {
        if (channelUrl == null || channelUrl.isEmpty()){
            return;
        }

        // Hide add button
        addOne.setVisibility(View.GONE);

        // Release existing player if it exists
        playerRelease(exoPlayerOne);

        // Build ExoPlayer instance
        exoPlayerOne = new ExoPlayer.Builder(this, renderersFactory)
                .setTrackSelector(getTrackSelector())
                .setMediaSourceFactory(new DefaultMediaSourceFactory(this, extractorsFactory))
                .build();

        exoPlayerOne.setHandleAudioBecomingNoisy(!isTvBox);

        // Set up PlayerView for the ExoPlayer
        PlayerView playerView = findViewById(R.id.player_one);
        playerView.setPlayer(exoPlayerOne);
        playerView.setUseController(true);
        playerView.requestFocus();

        playerView.setResizeMode(AspectRatioFrameLayout.RESIZE_MODE_FIT);
        exoPlayerOne.setVideoScalingMode(C.VIDEO_SCALING_MODE_DEFAULT);

        // Add listener to ExoPlayer to handle player state changes
        exoPlayerOne.addListener(new Player.Listener(){
            @Override
            public void onPlaybackStateChanged(int playbackState) {
                Player.Listener.super.onPlaybackStateChanged(playbackState);
                if (playbackState == Player.STATE_READY) {
                    findViewById(R.id.vw_player_one).setVisibility(View.VISIBLE);
                    findViewById(R.id.player_one).setVisibility(View.VISIBLE);
                    volumeOne.setVisibility(View.VISIBLE);
                    isPlayer = false;
                }

                // Handle buffering state
                if (playbackState == PlaybackStateEvent.STATE_PLAYING) {
                    pbOne.setVisibility(View.GONE);
                    playbackOne = 1;
                } else if (playbackState == Player.STATE_BUFFERING) {
                    pbOne.setVisibility(View.VISIBLE);
                }
            }

            @Override
            public void onPlayerError(@NonNull PlaybackException error) {
                Player.Listener.super.onPlayerError(error);
                if (isFinishing()){
                    return;
                }
                if (playbackOne < MAX_RETRIES){
                    playbackOne = playbackOne + 1;
                    Toast.makeText(MultipleScreenActivity.this,TAG_PLAYBACK_ERROR + playbackOne + "/5 ", Toast.LENGTH_SHORT).show();
                    new Handler(Looper.getMainLooper()).postDelayed(() -> {
                        if (isFinishing()){
                            return;
                        }
                        setPlayerOne(urlOne);
                    }, 600);
                    return;
                }

                playbackOne = 1;
                playerRelease(exoPlayerOne);
                pbOne.setVisibility(View.GONE);
                addOne.setVisibility(View.VISIBLE);
                findViewById(R.id.vw_player_one).setVisibility(View.INVISIBLE);
                findViewById(R.id.player_one).setVisibility(View.GONE);
                Toast.makeText(MultipleScreenActivity.this, error.getMessage(), Toast.LENGTH_SHORT).show();
            }
        });

        // Prepare media source and start playing
        Uri uri = Uri.parse(channelUrl);
        MediaSource mediaSource = buildMediaSource(uri);
        exoPlayerOne.setMediaSource(mediaSource);
        exoPlayerOne.prepare();
        exoPlayerOne.setPlayWhenReady(true);
        exoPlayerOne.setVolume(0); // Mute player initially

        setVolume();
    }

    private void setVolume() {
        // If is_player is true, set volume to 1 (unmute)
        if (isPlayer){
            setVolume(PlayerNumber.ONE);
        }
    }

    private void setPlayerTow(String channelUrl) {
        if (channelUrl == null || channelUrl.isEmpty()){
            return;
        }

        // Hide add button
        addTwo.setVisibility(View.GONE);

        // Release existing player if it exists
        playerRelease(exoPlayerTwo);

        // Build ExoPlayer instance
        exoPlayerTwo = new ExoPlayer.Builder(this, renderersFactory)
                .setTrackSelector(getTrackSelector())
                .setMediaSourceFactory(new DefaultMediaSourceFactory(this, extractorsFactory))
                .build();

        exoPlayerTwo.setHandleAudioBecomingNoisy(!isTvBox);

        // Set up PlayerView for the ExoPlayer
        PlayerView playerView = findViewById(R.id.player_tow);
        playerView.setPlayer(exoPlayerTwo);
        playerView.setUseController(true);
        playerView.requestFocus();

        playerView.setResizeMode(AspectRatioFrameLayout.RESIZE_MODE_FIT);
        exoPlayerTwo.setVideoScalingMode(C.VIDEO_SCALING_MODE_DEFAULT);

        // Add listener to ExoPlayer to handle player state changes
        exoPlayerTwo.addListener(new Player.Listener(){
            @Override
            public void onPlaybackStateChanged(int playbackState) {
                Player.Listener.super.onPlaybackStateChanged(playbackState);
                if (playbackState == Player.STATE_READY) {
                    findViewById(R.id.vw_player_tow).setVisibility(View.VISIBLE);
                    findViewById(R.id.player_tow).setVisibility(View.VISIBLE);
                    volumeTwo.setVisibility(View.VISIBLE);
                }

                // Handle buffering state
                if (playbackState == PlaybackStateEvent.STATE_PLAYING) {
                    pbTwo.setVisibility(View.GONE);
                    playbackTwo = 1;
                } else if (playbackState == Player.STATE_BUFFERING) {
                    pbTwo.setVisibility(View.VISIBLE);
                }
            }

            @Override
            public void onPlayerError(@NonNull PlaybackException error) {
                Player.Listener.super.onPlayerError(error);
                if (isFinishing()){
                    return;
                }
                if (playbackTwo < MAX_RETRIES){
                    playbackTwo = playbackTwo + 1;
                    Toast.makeText(MultipleScreenActivity.this,TAG_PLAYBACK_ERROR + playbackTwo + "/5 ", Toast.LENGTH_SHORT).show();
                    new Handler(Looper.getMainLooper()).postDelayed(() -> {
                        if (isFinishing()){
                            return;
                        }
                        setPlayerTow(urlTwo);
                    }, 600);
                    return;
                }

                playbackTwo = 1;
                playerRelease(exoPlayerTwo);
                pbTwo.setVisibility(View.GONE);
                addTwo.setVisibility(View.VISIBLE);
                findViewById(R.id.vw_player_tow).setVisibility(View.INVISIBLE);
                findViewById(R.id.player_tow).setVisibility(View.GONE);
                Toast.makeText(MultipleScreenActivity.this, error.getMessage(), Toast.LENGTH_SHORT).show();
            }
        });

        // Prepare media source and start playing
        Uri uri = Uri.parse(channelUrl);
        MediaSource mediaSource = buildMediaSource(uri);
        exoPlayerTwo.setMediaSource(mediaSource);
        exoPlayerTwo.prepare();
        exoPlayerTwo.setPlayWhenReady(true);
        exoPlayerTwo.setVolume(0); // Mute player initially
    }

    private void setPlayerThree(String channelUrl) {
        if (channelUrl == null || channelUrl.isEmpty()){
            return;
        }

        // Hide add button
        addThree.setVisibility(View.GONE);

        // Release existing player if it exists
        playerRelease(exoPlayerThree);

        // Build ExoPlayer instance
        exoPlayerThree = new ExoPlayer.Builder(this, renderersFactory)
                .setTrackSelector(getTrackSelector())
                .setMediaSourceFactory(new DefaultMediaSourceFactory(this, extractorsFactory))
                .build();

        exoPlayerThree.setHandleAudioBecomingNoisy(!isTvBox);

        // Set up PlayerView for the ExoPlayer
        PlayerView playerView = findViewById(R.id.player_three);
        playerView.setPlayer(exoPlayerThree);
        playerView.setUseController(true);
        playerView.requestFocus();

        playerView.setResizeMode(AspectRatioFrameLayout.RESIZE_MODE_FIT);
        exoPlayerThree.setVideoScalingMode(C.VIDEO_SCALING_MODE_DEFAULT);

        // Add listener to ExoPlayer to handle player state changes
        exoPlayerThree.addListener(new Player.Listener(){
            @Override
            public void onPlaybackStateChanged(int playbackState) {
                Player.Listener.super.onPlaybackStateChanged(playbackState);
                if (playbackState == Player.STATE_READY) {
                    findViewById(R.id.vw_player_three).setVisibility(View.VISIBLE);
                    volumeThree.setVisibility(View.VISIBLE);
                    findViewById(R.id.player_three).setVisibility(View.VISIBLE);
                }

                // Handle buffering state
                if (playbackState == PlaybackStateEvent.STATE_PLAYING) {
                    pbThree.setVisibility(View.GONE);
                    playbackThree = 1;
                } else if (playbackState == Player.STATE_BUFFERING) {
                    pbThree.setVisibility(View.VISIBLE);
                }
            }

            @Override
            public void onPlayerError(@NonNull PlaybackException error) {
                Player.Listener.super.onPlayerError(error);
                if (isFinishing()){
                    return;
                }
                if (playbackThree < MAX_RETRIES){
                    playbackThree = playbackThree + 1;
                    Toast.makeText(MultipleScreenActivity.this,TAG_PLAYBACK_ERROR + playbackThree + "/5 ", Toast.LENGTH_SHORT).show();
                    new Handler(Looper.getMainLooper()).postDelayed(() -> {
                        if (isFinishing()){
                            return;
                        }
                        setPlayerThree(urlThree);
                    }, 600);
                    return;
                }

                playbackThree = 1;
                playerRelease(exoPlayerThree);
                pbThree.setVisibility(View.GONE);
                addThree.setVisibility(View.VISIBLE);
                findViewById(R.id.vw_player_three).setVisibility(View.INVISIBLE);
                findViewById(R.id.player_three).setVisibility(View.GONE);
                Toast.makeText(MultipleScreenActivity.this, error.getMessage(), Toast.LENGTH_SHORT).show();
            }
        });

        // Prepare media source and start playing
        Uri uri = Uri.parse(channelUrl);
        MediaSource mediaSource = buildMediaSource(uri);
        exoPlayerThree.setMediaSource(mediaSource);
        exoPlayerThree.prepare();
        exoPlayerThree.setPlayWhenReady(true);
        exoPlayerThree.setVolume(0); // Mute player initially
    }

    private void setPlayerFour(String channelUrl) {
        if (channelUrl == null || channelUrl.isEmpty()){
            return;
        }

        // Hide add button
        addFour.setVisibility(View.GONE);

        // Release existing player if it exists
        playerRelease(exoPlayerFour);

        // Build ExoPlayer instance
        exoPlayerFour = new ExoPlayer.Builder(this, renderersFactory)
                .setTrackSelector(getTrackSelector())
                .setMediaSourceFactory(new DefaultMediaSourceFactory(this, extractorsFactory))
                .build();

        exoPlayerFour.setHandleAudioBecomingNoisy(!isTvBox);

        // Set up PlayerView for the ExoPlayer
        PlayerView playerView = findViewById(R.id.player_four);
        playerView.setPlayer(exoPlayerFour);
        playerView.setUseController(true);
        playerView.requestFocus();

        playerView.setResizeMode(AspectRatioFrameLayout.RESIZE_MODE_FIT);
        exoPlayerFour.setVideoScalingMode(C.VIDEO_SCALING_MODE_DEFAULT);

        // Add listener to ExoPlayer to handle player state changes
        exoPlayerFour.addListener(new Player.Listener(){
            @Override
            public void onPlaybackStateChanged(int playbackState) {
                Player.Listener.super.onPlaybackStateChanged(playbackState);
                if (playbackState == Player.STATE_READY) {
                    findViewById(R.id.vw_player_four).setVisibility(View.VISIBLE);
                    volumeFour.setVisibility(View.VISIBLE);
                    findViewById(R.id.player_four).setVisibility(View.VISIBLE);
                }

                // Handle buffering state
                if (playbackState == PlaybackStateEvent.STATE_PLAYING) {
                    pbFour.setVisibility(View.GONE);
                    playbackFour = 1;
                } else if (playbackState == Player.STATE_BUFFERING) {
                    pbFour.setVisibility(View.VISIBLE);
                }
            }

            @Override
            public void onPlayerError(@NonNull PlaybackException error) {
                Player.Listener.super.onPlayerError(error);
                if (isFinishing()){
                    return;
                }
                if (playbackFour < MAX_RETRIES){
                    playbackFour = playbackFour + 1;
                    Toast.makeText(MultipleScreenActivity.this,TAG_PLAYBACK_ERROR + playbackFour + "/5 ", Toast.LENGTH_SHORT).show();
                    new Handler(Looper.getMainLooper()).postDelayed(() -> {
                        if (isFinishing()){
                            return;
                        }
                        setPlayerFour(urlFour);
                    }, 600);
                    return;
                }

                playbackFour = 1;
                playerRelease(exoPlayerFour);
                pbFour.setVisibility(View.GONE);
                addFour.setVisibility(View.VISIBLE);
                findViewById(R.id.vw_player_four).setVisibility(View.INVISIBLE);
                findViewById(R.id.player_four).setVisibility(View.GONE);
                Toast.makeText(MultipleScreenActivity.this, error.getMessage(), Toast.LENGTH_SHORT).show();
            }
        });

        // Prepare media source and start playing
        Uri uri = Uri.parse(channelUrl);
        MediaSource mediaSource = buildMediaSource(uri);
        exoPlayerFour.setMediaSource(mediaSource);
        exoPlayerFour.prepare();
        exoPlayerFour.setPlayWhenReady(true);
        exoPlayerFour.setVolume(0); // Mute player initially
    }

    private void playerRelease(ExoPlayer exoPlayer) {
        try {
            if (exoPlayer != null) {
                exoPlayer.setPlayWhenReady(false);
                exoPlayer.stop();
                exoPlayer.release();
            }
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "Error releasing player",e);
        }
    }

    @NonNull
    private TrackSelector getTrackSelector() {
        DefaultTrackSelector trackSelector = new DefaultTrackSelector(this);
        // Set captioning parameters if enabled
        final CaptioningManager captioningManager = (CaptioningManager) getSystemService(Context.CAPTIONING_SERVICE);
        if (!captioningManager.isEnabled()) {
            trackSelector.setParameters(trackSelector.buildUponParameters().setIgnoredTextSelectionFlags(C.SELECTION_FLAG_DEFAULT));
        }
        Locale locale = captioningManager.getLocale();
        if (locale != null) {
            trackSelector.setParameters(trackSelector.buildUponParameters().setPreferredTextLanguage(locale.getISO3Language()));
        }
        return trackSelector;
    }

    @NonNull
    private MediaSource buildMediaSource(Uri uri) {
        int type = Util.inferContentType(uri);
        MediaItem mediaItem = MediaItem.fromUri(uri);
        return switch (type) {
            case C.CONTENT_TYPE_SS ->
                // For SmoothStreaming (SS)
                    new SsMediaSource.Factory(new DefaultSsChunkSource.Factory(mediaDataSourceFactory),
                            buildDataSourceFactory(false)).createMediaSource(mediaItem);
            case C.CONTENT_TYPE_DASH ->
                // For Dynamic Adaptive Streaming over HTTP (DASH)
                    new DashMediaSource.Factory(new DefaultDashChunkSource.Factory(mediaDataSourceFactory),
                            buildDataSourceFactory(false)).createMediaSource(mediaItem);
            case C.CONTENT_TYPE_HLS ->
                // For HTTP Live Streaming (HLS)
                    new HlsMediaSource.Factory(mediaDataSourceFactory).createMediaSource(mediaItem);
            case C.CONTENT_TYPE_RTSP ->
                // For Real-Time Streaming Protocol (RTSP)
                    new RtspMediaSource.Factory().createMediaSource(mediaItem);
            case C.CONTENT_TYPE_OTHER ->
                // For Progressive Media
                    new ProgressiveMediaSource.Factory(mediaDataSourceFactory).createMediaSource(mediaItem);
            default ->
                    new ProgressiveMediaSource.Factory(mediaDataSourceFactory).createMediaSource(mediaItem);
        };
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
        return new DefaultHttpDataSource.Factory().setUserAgent(spHelper.getAgentName().isEmpty()
                        ? Util.getUserAgent(MultipleScreenActivity.this, "ExoPlayerDemo")
                        : spHelper.getAgentName())
                .setTransferListener(bandwidthMeter)
                .setAllowCrossProtocolRedirects(true)
                .setKeepPostFor302Redirects(true);
    }

    private void releasePlayer(ExoPlayer exoPlayer, ProgressBar progressBar, ImageView addButton,
                               int volumeView, int playerView, ImageView volumeIcon) {
        try {
            if (exoPlayer != null) {
                // Stop and release the ExoPlayer instance
                exoPlayer.setPlayWhenReady(false);
                exoPlayer.stop();
                exoPlayer.release();
            }
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "Error releasing player",e);
        }

        // Update UI elements after releasing player
        progressBar.setVisibility(View.GONE);
        addButton.setVisibility(View.VISIBLE);
        findViewById(volumeView).setVisibility(View.INVISIBLE);
        findViewById(playerView).setVisibility(View.GONE);
        volumeIcon.setVisibility(View.GONE);

        // Show a toast message indicating player release
        Toast.makeText(MultipleScreenActivity.this,"Removed", Toast.LENGTH_LONG).show();
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_multiple_screen;
    }

    @Override
    public void onStop() {
        super.onStop();
        if (exoPlayerOne != null && exoPlayerOne.getPlayWhenReady()) {
            exoPlayerOne.setPlayWhenReady(false);
            exoPlayerOne.getPlaybackState();
        }
        if (exoPlayerTwo != null && exoPlayerTwo.getPlayWhenReady()) {
            exoPlayerTwo.setPlayWhenReady(false);
            exoPlayerTwo.getPlaybackState();
        }
        if (exoPlayerThree != null && exoPlayerThree.getPlayWhenReady()) {
            exoPlayerThree.setPlayWhenReady(false);
            exoPlayerThree.getPlaybackState();
        }
        if (exoPlayerFour != null && exoPlayerFour.getPlayWhenReady()) {
            exoPlayerFour.setPlayWhenReady(false);
            exoPlayerFour.getPlaybackState();
        }
    }

    @Override
    public void onPause() {
        super.onPause();
        if (exoPlayerOne != null && exoPlayerOne.getPlayWhenReady()) {
            exoPlayerOne.setPlayWhenReady(false);
            exoPlayerOne.getPlaybackState();
        }
        if (exoPlayerTwo != null && exoPlayerTwo.getPlayWhenReady()) {
            exoPlayerTwo.setPlayWhenReady(false);
            exoPlayerTwo.getPlaybackState();
        }
        if (exoPlayerThree != null && exoPlayerThree.getPlayWhenReady()) {
            exoPlayerThree.setPlayWhenReady(false);
            exoPlayerThree.getPlaybackState();
        }
        if (exoPlayerFour != null && exoPlayerFour.getPlayWhenReady()) {
            exoPlayerFour.setPlayWhenReady(false);
            exoPlayerFour.getPlaybackState();
        }
    }

    @Override
    public void onResume() {
        super.onResume();
        if (exoPlayerOne != null) {
            exoPlayerOne.setPlayWhenReady(true);
            exoPlayerOne.getPlaybackState();
        }
        if (exoPlayerTwo != null) {
            exoPlayerTwo.setPlayWhenReady(true);
            exoPlayerTwo.getPlaybackState();
        }
        if (exoPlayerThree != null) {
            exoPlayerThree.setPlayWhenReady(true);
            exoPlayerThree.getPlaybackState();
        }
        if (exoPlayerFour != null) {
            exoPlayerFour.setPlayWhenReady(true);
            exoPlayerFour.getPlaybackState();
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        if (exoPlayerOne != null) {
            exoPlayerOne.setPlayWhenReady(false);
            exoPlayerOne.stop();
            exoPlayerOne.release();
        }
        if (exoPlayerTwo != null) {
            exoPlayerTwo.setPlayWhenReady(false);
            exoPlayerTwo.stop();
            exoPlayerTwo.release();
        }
        if (exoPlayerThree != null) {
            exoPlayerThree.setPlayWhenReady(false);
            exoPlayerThree.stop();
            exoPlayerThree.release();
        }
        if (exoPlayerFour != null) {
            exoPlayerFour.setPlayWhenReady(false);
            exoPlayerFour.stop();
            exoPlayerFour.release();
        }
    }

    private final ActivityResultLauncher<Intent> filterLauncherPlayerOne = registerForActivityResult(
            new ActivityResultContracts.StartActivityForResult(), result -> {
                if (result.getResultCode() == Activity.RESULT_OK && result.getData() != null) {
                    urlOne = getChannelUrl(result.getData().getStringExtra(TAG_STREAM_ID));
                    setPlayerOne(urlOne);
                }
            }
    );

    private final ActivityResultLauncher<Intent> filterLauncherPlayerTwo = registerForActivityResult(
            new ActivityResultContracts.StartActivityForResult(), result -> {
                if (result.getResultCode() == Activity.RESULT_OK && result.getData() != null) {
                    urlTwo = getChannelUrl(result.getData().getStringExtra(TAG_STREAM_ID));
                    setPlayerTow(urlTwo);
                }
            }
    );

    private final ActivityResultLauncher<Intent> filterLauncherPlayerThree = registerForActivityResult(
            new ActivityResultContracts.StartActivityForResult(), result -> {
                if (result.getResultCode() == Activity.RESULT_OK && result.getData() != null) {
                    urlThree = getChannelUrl(result.getData().getStringExtra(TAG_STREAM_ID));
                    setPlayerThree(urlThree);
                }
            }
    );

    private final ActivityResultLauncher<Intent> filterLauncherPlayerFour = registerForActivityResult(
            new ActivityResultContracts.StartActivityForResult(), result -> {
                if (result.getResultCode() == Activity.RESULT_OK && result.getData() != null) {
                    urlFour = getChannelUrl(result.getData().getStringExtra(TAG_STREAM_ID));
                    setPlayerFour(urlFour);
                }
            }
    );

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && keyCode == KeyEvent.KEYCODE_BACK){
            finish();
            return true;
        } else if (event.getAction() == KeyEvent.ACTION_DOWN && keyCode == KeyEvent.KEYCODE_HOME){
            ThemeHelper.openHomeActivity(this);
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }
}