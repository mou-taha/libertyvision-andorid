package nemosofts.streambox.utils.player;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.res.Resources;
import android.graphics.Color;
import android.graphics.drawable.Drawable;
import android.media.AudioManager;
import android.util.AttributeSet;
import android.view.GestureDetector;
import android.view.MotionEvent;
import android.view.ScaleGestureDetector;
import android.view.View;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.media3.ui.PlayerView;

import java.util.Locale;

import nemosofts.streambox.R;
import androidx.nemosofts.utils.DeviceUtils;

@OptIn(markerClass = androidx.media3.common.util.UnstableApi.class)
public class CustomPlayerView extends PlayerView implements GestureDetector.OnGestureListener{

    public static final String BRIGHTNESS_AUTO = "auto";
    public static final String BRIGHTNESS_MEDIUM = "medium";

    public static final String VOLUME_UP = "volume_up";
    public static final String VOLUME_DOWN = "volume_down";
    public static final String VOLUME_OFF = "volume_off";

    private float gestureScrollY = 0f;
    private float gestureScrollX = 0f;
    private boolean handleTouch;
    private boolean isScaling;

    private final float ignoreBorder = dpToPx(24);

    public static final int MESSAGE_TIMEOUT_TOUCH = 400;
    public static final int MESSAGE_TIMEOUT_KEY = 800;

    private final AudioManager mAudioManager;
    private BrightnessVolumeControl brightnessControl;

    private final TextView exoErrorMessage;

    private final GestureDetector mDetector;
    private final ScaleGestureDetector scaleGestureDetector;
    private final boolean isPinchEnabled;

    private static int volumeUpsInRow = 0;
    public static int getVolumeUpsInRow() {
        return volumeUpsInRow;
    }
    public static void setVolumeUpsInRow(int volume) {
        volumeUpsInRow = volume;
    }
    public static void incrementVolumeUpsInRow() {
        volumeUpsInRow++;
    }
    public static void resetVolumeUpsInRow() {
        volumeUpsInRow = 0;
    }

    public final Runnable textClearRunnable = () -> {
        setCustomErrorMessage(null);
        clearIcon();
    };

    public CustomPlayerView(Context context) {
        this(context, null);
    }

    public CustomPlayerView(Context context, AttributeSet attrs) {
        this(context, attrs, 0);
    }

    public CustomPlayerView(Context context, AttributeSet attrs, int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        mDetector = new GestureDetector(context, this);
        mAudioManager = (AudioManager) context.getSystemService(Context.AUDIO_SERVICE);
        exoErrorMessage = findViewById(androidx.media3.ui.R.id.exo_error_message);
        isPinchEnabled = !DeviceUtils.isTvBox(context);
        scaleGestureDetector = isPinchEnabled ? new ScaleGestureDetector(context, new PinchGestureListener()) : null;
    }

    public void setHighlight(boolean active) {
        if (exoErrorMessage == null){
            return;
        }
        Drawable background = exoErrorMessage.getBackground();
        if (background == null){
            return;
        }

        if (active){
            background.setTint(Color.RED);
        } else{
            background.setTintList(null);
        }
    }

    public void setIconBrightness(String type) {
        if (exoErrorMessage == null){
            return;
        }
        exoErrorMessage.setCompoundDrawablesWithIntrinsicBounds(
                BRIGHTNESS_MEDIUM.equals(type)
                        ? R.drawable.ic_brightness_medium
                        : R.drawable.ic_brightness_auto
                , 0, 0, 0);
    }

    public void setIconVolume(boolean volumeActive) {
        setIconVolume(volumeActive ? CustomPlayerView.VOLUME_UP : CustomPlayerView.VOLUME_OFF);
    }

    public void setIconVolume(String type) {
        if (exoErrorMessage == null){
            return;
        }
        if (VOLUME_UP.equals(type)){
            exoErrorMessage.setCompoundDrawablesWithIntrinsicBounds(R.drawable.ic_volume_up, 0, 0, 0);
        } else  if (VOLUME_DOWN.equals(type)){
            exoErrorMessage.setCompoundDrawablesWithIntrinsicBounds(R.drawable.ic_volume_down, 0, 0, 0);
        } else {
            exoErrorMessage.setCompoundDrawablesWithIntrinsicBounds(R.drawable.ic_volume_off, 0, 0, 0);
        }
    }

    public void setBrightnessControl(BrightnessVolumeControl brightnessControl) {
        this.brightnessControl = brightnessControl;
    }

    public void clearIcon() {
        if (exoErrorMessage == null){
            return;
        }
        exoErrorMessage.setCompoundDrawablesWithIntrinsicBounds(0, 0, 0, 0);
        setHighlight(false);
    }

    @Override
    public boolean onDown(@NonNull MotionEvent motionEvent) {
        gestureScrollY = 0;
        gestureScrollX = 0;
        return false;
    }

    @Override
    public void onShowPress(@NonNull MotionEvent e) {
        // Add comment explaining why this method is empty or implement functionality if necessary
    }

    @Override
    public boolean onSingleTapUp(@NonNull MotionEvent e) {
        // Implement logic if necessary or return true/false based on your requirements
        return false;
    }


    @SuppressLint("UnsafeOptInUsageError")
    @Override
    public boolean onScroll(MotionEvent motionEvent, @NonNull MotionEvent motionEvent1, float distanceX, float distanceY) {
        if (motionEvent == null || isScaling || brightnessControl == null) {
            return false; // Early return if motionEvent is null
        }

        // Exclude edge areas
        if (motionEvent.getY() < ignoreBorder || motionEvent.getX() < ignoreBorder ||
                motionEvent.getY() > getHeight() - ignoreBorder || motionEvent.getX() > getWidth() - ignoreBorder)
            return false;

        // Initialize gesture scroll values if they are still zero
        if (gestureScrollY == 0 || gestureScrollX == 0) {
            gestureScrollY = 0.0001f;
            gestureScrollX = 0.0001f;
            return false;
        }

        // LEFT = Brightness  |  RIGHT = Volume
        gestureScrollY += distanceY;
        if (Math.abs(gestureScrollY) > dpToPx(16)) {
            if (motionEvent.getX() < ((float) getWidth() / 2)) {
                brightnessControl.changeBrightness(this, gestureScrollY > 0, false);
            } else {
                brightnessControl.adjustVolume(mAudioManager, this, gestureScrollY > 0);
            }
            // Reset gesture scroll after adjustment
            gestureScrollY = 0.0001f;
        }
        return true;
    }

    @Override
    public void onLongPress(@NonNull MotionEvent e) {
        // Add comment explaining why this method is empty or implement functionality if necessary
    }

    @Override
    public boolean onFling(MotionEvent e1, @NonNull MotionEvent e2, float velocityX, float velocityY) {
        // Implement logic if necessary or return true/false based on your requirements
        return false;
    }

    @SuppressLint("ClickableViewAccessibility")
    @Override
    public boolean onTouchEvent(MotionEvent event) {
        if (event != null){
            boolean scaleHandled = scaleGestureDetector != null && scaleGestureDetector.onTouchEvent(event);
            if (event.getActionMasked() == MotionEvent.ACTION_DOWN){
                removeCallbacks(textClearRunnable);
                handleTouch = true;
            } else if (event.getActionMasked() == MotionEvent.ACTION_UP || event.getActionMasked() == MotionEvent.ACTION_CANCEL && (handleTouch)){
                postDelayed(textClearRunnable, MESSAGE_TIMEOUT_TOUCH);
                setControllerAutoShow(true);
            }

            if (handleTouch && !isScaling){
                mDetector.onTouchEvent(event);
            }
            if (scaleHandled && isScaling) {
                // consume while scaling to avoid brightness/volume gestures
                return super.onTouchEvent(event);
            }
        }
        return super.onTouchEvent(event);
    }

    public static int dpToPx(int dp) {
        return (int) (dp * Resources.getSystem().getDisplayMetrics().density);
    }

    public View getZoomTarget() {
        View view = getVideoSurfaceView();
        if (view == null) {
            view = findViewById(androidx.media3.ui.R.id.exo_content_frame);
        }
        return view;
    }

    private class PinchGestureListener extends ScaleGestureDetector.SimpleOnScaleGestureListener {
        private static final float MIN_SCALE = 1f;
        private static final float MAX_SCALE = 2.5f;
        private float currentScale = 1f;

        @Override
        public boolean onScaleBegin(@NonNull ScaleGestureDetector detector) {
            isScaling = true;
            removeCallbacks(textClearRunnable);
            return true;
        }

        @Override
        public boolean onScale(@NonNull ScaleGestureDetector detector) {
            View target = getZoomTarget();
            if (target == null) {
                return false;
            }
            currentScale *= detector.getScaleFactor();
            currentScale = Math.max(MIN_SCALE, Math.min(currentScale, MAX_SCALE));
            target.setPivotX(detector.getFocusX());
            target.setPivotY(detector.getFocusY());
            target.setScaleX(currentScale);
            target.setScaleY(currentScale);
            if (exoErrorMessage != null){
                setHighlight(true);
                exoErrorMessage.setText(String.format(Locale.getDefault(), "%d%%", Math.round(currentScale * 100)));
            }
            return true;
        }

        @Override
        public void onScaleEnd(@NonNull ScaleGestureDetector detector) {
            super.onScaleEnd(detector);
            postDelayed(textClearRunnable, MESSAGE_TIMEOUT_TOUCH);
            isScaling = false;
        }
    }
}
