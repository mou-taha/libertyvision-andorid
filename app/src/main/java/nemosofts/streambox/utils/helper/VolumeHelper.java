package nemosofts.streambox.utils.helper;

import android.content.Context;
import android.media.AudioManager;
import android.media.audiofx.LoudnessEnhancer;
import android.os.Build;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.annotation.OptIn;
import androidx.media3.common.util.UnstableApi;

import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.Objects;

import nemosofts.streambox.utils.player.CustomPlayerView;


public class VolumeHelper {

    private static final String TAG = "VolumeHelper";
    private static final int SAMSUNG_MAX_FINE_VOLUME = 150;
    private static final int BOOST_GAIN_MULTIPLIER = 200;
    private static final int MAX_BOOST_LEVEL = 10;
    private static final int VOLUME_UP_THRESHOLD = 4;
    private static final int DEFAULT_SAMSUNG_INTERVAL = 10;
    private static final int VOLUME_CHANGE_FLAGS =
            AudioManager.FLAG_REMOVE_SOUND_AND_VIBRATE | AudioManager.FLAG_SHOW_UI;

    private VolumeHelper() {
        throw new AssertionError("No VolumeHelper instances for you!");
    }

    /**
     * Gets the current or maximum volume level for the music stream.
     * <p>
     * For Samsung devices running Android 11 or above, uses proprietary APIs
     * to get more precise volume control when available.
     * </p>
     *
     * @param context      The application context
     * @param max          Whether to return the maximum volume level (true) or current volume (false)
     * @param audioManager The AudioManager instance
     * @return The current or maximum volume level
     * @throws NullPointerException if audioManager is null
     */
    public static int getVolume(Context context,
                                boolean max,
                                AudioManager audioManager) {

        Objects.requireNonNull(audioManager, "AudioManager must not be null");

        if (isSamsungDeviceWithSDK30OrAbove()) {
            Integer samsungVolume = getSamsungVolume(context, max, audioManager);
            if (samsungVolume != null) {
                return samsungVolume;
            }
        }

        return max ? audioManager.getStreamMaxVolume(AudioManager.STREAM_MUSIC)
                : audioManager.getStreamVolume(AudioManager.STREAM_MUSIC);
    }

    /**
     * Handles volume step changes with special behavior for consecutive increases.
     * <p>
     * Implements a "force volume up" feature when the volume button is pressed repeatedly.
     * Updates the player view with the current volume level.
     * </p>
     *
     * @param context      The application context
     * @param audioManager The AudioManager instance
     * @param playerView   The player view to display volume changes (may be null)
     * @param raise        Whether to increase volume (true) or decrease volume (false)
     * @param oldVolume    The previous volume level before adjustment
     * @throws NullPointerException if audioManager is null
     */
    @OptIn(markerClass = UnstableApi.class)
    public static void handleVolumeStep(Context context,
                                        AudioManager audioManager,
                                        CustomPlayerView playerView,
                                        boolean raise,
                                        int oldVolume) {

        Objects.requireNonNull(audioManager, "AudioManager must not be null");

        audioManager.adjustStreamVolume(
                AudioManager.STREAM_MUSIC,
                raise ? AudioManager.ADJUST_RAISE : AudioManager.ADJUST_LOWER,
                AudioManager.FLAG_REMOVE_SOUND_AND_VIBRATE
        );

        final int currentVolume = getVolume(context, false, audioManager);
        if (raise && oldVolume == currentVolume) {
            CustomPlayerView.incrementVolumeUpsInRow();
        } else {
            CustomPlayerView.resetVolumeUpsInRow();
        }

        if (shouldForceVolumeIncrease() && !isVolumeMin(audioManager)) {
            audioManager.adjustStreamVolume(
                    AudioManager.STREAM_MUSIC, AudioManager.ADJUST_RAISE, VOLUME_CHANGE_FLAGS
            );
        } else if (playerView != null){
            playerView.setCustomErrorMessage(currentVolume != 0 ? " " + currentVolume : "");
        }
    }

    /**
     * Handles audio boost functionality using LoudnessEnhancer.
     * <p>
     * Adjusts the boost level and updates the player view with the combined volume+boost level.
     * </p>
     *
     * @param playerView       The player view to display boost level (may be null)
     * @param loudnessEnhancer The LoudnessEnhancer instance (may be null)
     * @param boostLevel       Current boost level (0 to MAX_BOOST_LEVEL)
     * @param raise            Whether to increase boost (true) or decrease boost (false)
     * @param canBoost         Whether boosting is currently allowed
     * @param maxVolume        The maximum volume level without boost
     */
    @OptIn(markerClass = UnstableApi.class)
    public static void handleBoost(CustomPlayerView playerView,
                                   LoudnessEnhancer loudnessEnhancer,
                                   int boostLevel,
                                   boolean raise,
                                   boolean canBoost,
                                   int maxVolume) {

        int newBoostLevel = boostLevel;

        if (canBoost && raise && boostLevel < MAX_BOOST_LEVEL) {
            newBoostLevel++;
        } else if (!raise && boostLevel > 0) {
            newBoostLevel--;
        }

        if (loudnessEnhancer != null) {
            try {
                loudnessEnhancer.setTargetGain(newBoostLevel * BOOST_GAIN_MULTIPLIER);
                loudnessEnhancer.setEnabled(newBoostLevel > 0);
            } catch (Exception e) {
                Log.e(TAG, "Error in handleBoost", e);
            }
        }

        if (playerView != null) {
            playerView.setCustomErrorMessage(" " + (maxVolume + newBoostLevel));
        }
    }

    /**
     * Checks if the device is a Samsung device running Android 11 (API 30) or above.
     *
     * @return true if the device is a Samsung device with SDK 30+, false otherwise
     */
    private static boolean isSamsungDeviceWithSDK30OrAbove() {
        return Build.VERSION.SDK_INT >= Build.VERSION_CODES.R
                && "samsung".equalsIgnoreCase(Build.MANUFACTURER);
    }

    /**
     * Gets the volume level for Samsung devices using proprietary APIs.
     *
     * @param context      The application context
     * @param max          Whether to return the maximum volume level
     * @param audioManager The AudioManager instance
     * @return The volume level, or null if not a Samsung device or API call fails
     */
    @Nullable
    private static Integer getSamsungVolume(Context context,
                                            boolean max,
                                            AudioManager audioManager) {
        try {
            int mediaVolumeInterval = getSamsungMediaVolumeInterval(context);
            if (mediaVolumeInterval < MAX_BOOST_LEVEL) {
                return retrieveFineVolume(max, audioManager, mediaVolumeInterval);
            }
        } catch (ReflectiveOperationException e) {
            Log.e(TAG, "Failed to get Samsung volume", e);
        }
        return null;
    }

    /**
     * Checks if consecutive volume increases should force a volume boost.
     *
     * @return true if the volume up button has been pressed more than VOLUME_UP_THRESHOLD times consecutively
     */
    private static boolean shouldForceVolumeIncrease() {
        return CustomPlayerView.getVolumeUpsInRow() > VOLUME_UP_THRESHOLD;
    }

    /**
     * Checks if the current volume is at its minimum level.
     *
     * @param audioManager The AudioManager instance
     * @return true if the volume is at minimum, false otherwise
     * @throws NullPointerException if audioManager is null
     */
    private static boolean isVolumeMin(@NonNull AudioManager audioManager) {
        int min = Build.VERSION.SDK_INT >= Build.VERSION_CODES.P ?
                audioManager.getStreamMinVolume(AudioManager.STREAM_MUSIC) : 0;
        return audioManager.getStreamVolume(AudioManager.STREAM_MUSIC) == min;
    }

    /**
     * Retrieves fine-grained volume level for Samsung devices using reflection.
     *
     * @param max                Whether to return the maximum volume level
     * @param audioManager       The AudioManager instance
     * @param mediaVolumeInterval The volume interval from Samsung's API
     * @return The volume level, or null if the reflection call fails
     * @throws ReflectiveOperationException if reflection calls fail
     */
    @Nullable
    private static Integer retrieveFineVolume(boolean max,
                                              AudioManager audioManager,
                                              int mediaVolumeInterval)
            throws ReflectiveOperationException {
        String name = "semGetFineVolume";
        Method method = AudioManager.class.getDeclaredMethod(name, int.class);
        Object result = method.invoke(audioManager, AudioManager.STREAM_MUSIC);
        if (result instanceof Integer) {
            int fineVolume = (int) result;
            return max ? SAMSUNG_MAX_FINE_VOLUME / mediaVolumeInterval : fineVolume / mediaVolumeInterval;
        }
        return null;
    }

    /**
     * Gets the media volume interval for Samsung devices using reflection.
     *
     * @param context The application context
     * @return The media volume interval, or DEFAULT_SAMSUNG_INTERVAL if the call fails
     * @throws ReflectiveOperationException if reflection calls fail
     */
    private static int getSamsungMediaVolumeInterval(Context context)
            throws ReflectiveOperationException {
        Class<?> clazz = Class.forName("com.samsung.android.media.SemSoundAssistantManager");
        Constructor<?> constructor = clazz.getConstructor(Context.class);
        Object instance = constructor.newInstance(context);

        Method getMediaVolumeInterval = clazz.getDeclaredMethod("getMediaVolumeInterval");
        Object result = getMediaVolumeInterval.invoke(instance);

        return result instanceof Integer ? (int) result : DEFAULT_SAMSUNG_INTERVAL;
    }
}