package nemosofts.streambox.utils;

import android.app.Activity;
import android.content.Context;
import android.graphics.Color;
import android.net.ConnectivityManager;
import android.net.Network;
import android.net.NetworkCapabilities;
import android.os.Build;
import android.provider.Settings;
import android.util.Log;
import android.view.View;
import android.view.Window;
import android.view.WindowInsets;
import android.view.WindowInsetsController;
import android.view.WindowManager;

import androidx.core.view.WindowCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.core.view.WindowInsetsControllerCompat;

import nemosofts.streambox.utils.helper.SPHelper;


public class IfSupported {

    private static final String TAG = "IfSupported";

    private IfSupported() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Applies Right-to-Left (RTL) layout direction if enabled in preferences.
     * <p>
     * Also hides system bars on Android R (API 30) and above when RTL is enabled.
     * </p>
     *
     * @param activity The activity to apply RTL direction to
     */
    public static void isRTL(Activity activity) {
        if (activity == null) {
            ApplicationUtil.log(TAG, "Activity context is null in isRTL");
            return;
        }
        try {
            if (Boolean.TRUE.equals(new SPHelper(activity).getIsRTL())) {
                Window window = activity.getWindow();
                if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.R) {
                    WindowInsetsController insetsController = window.getInsetsController();
                    if (insetsController != null) {
                        insetsController.setSystemBarsBehavior(WindowInsetsController.BEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE);
                        insetsController.hide(WindowInsets.Type.systemBars());
                    }
                }
                window.getDecorView().setLayoutDirection(View.LAYOUT_DIRECTION_RTL);
            }
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "Failed to apply RTL layout direction", e);
        }
    }

    /**
     * Enables screenshot prevention if configured in preferences.
     * <p>
     * Sets the FLAG_SECURE window flag to prevent screenshots and screen recording.
     * </p>
     *
     * @param mContext The activity context to apply screenshot prevention to
     */
    public static void isScreenshot(Activity mContext) {
        if (mContext == null) {
            ApplicationUtil.log(TAG, "Activity context is null isScreenshot");
            return;
        }
        try {
            if (Boolean.TRUE.equals(new SPHelper(mContext).getIsScreenshot())) {
                Window window = mContext.getWindow();
                window.setFlags(WindowManager.LayoutParams.FLAG_SECURE, WindowManager.LayoutParams.FLAG_SECURE);
            }
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "Failed to isScreenshot", e);
        }
    }

    /**
     * Keeps the screen on for the specified activity.
     * <p>
     * Sets the FLAG_KEEP_SCREEN_ON window flag to prevent the screen from dimming or turning off.
     * </p>
     *
     * @param mContext The activity to keep the screen on for
     */
    public static void keepScreenOn(Activity mContext) {
        if (mContext == null) {
            ApplicationUtil.log(TAG, "Activity context is null keepScreenOn");
            return;
        }
        try {
            Window window = mContext.getWindow();
            window.addFlags(WindowManager.LayoutParams.FLAG_KEEP_SCREEN_ON);
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "Failed to keep screen on", e);
        }
    }

    /**
     * Hides the status bar for the specified activity.
     * <p>
     * Uses WindowCompat for backward compatibility.
     * </p>
     *
     * @param mContext The activity to hide the status bar for
     */
    public static void hideStatusBar(Activity mContext) {
        if (mContext == null) {
            ApplicationUtil.log(TAG, "Activity context is null hideStatusBar");
            return;
        }
        try {
            Window window = mContext.getWindow();
            hideStatusBarDialog(window);
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "Failed to hide status bar", e);
        }
    }

    /**
     * Hides the status bar for a given window/dialog.
     * <p>
     * Implements immersive mode with transient system bars that appear on swipe.
     * </p>
     *
     * @param window The window to hide the status bar for
     */
    public static void hideStatusBarDialog(Window window) {
        if (window == null) {
            Log.e(TAG, "Window is null");
            return;
        }
        try {
            View decorView = window.getDecorView();

            // Allow layout to extend behind system bars
            WindowCompat.setDecorFitsSystemWindows(window, false);

            // Use AndroidX controller to manage insets across all versions
            WindowInsetsControllerCompat controller =
                    new WindowInsetsControllerCompat(window, decorView);

            // Hide both status and navigation bars
            controller.hide(WindowInsetsCompat.Type.statusBars() | WindowInsetsCompat.Type.navigationBars());

            // Enable immersive sticky (swipe to show system bars temporarily)
            controller.setSystemBarsBehavior(
                    WindowInsetsControllerCompat.BEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE
            );

            if (Build.VERSION.SDK_INT <= Build.VERSION_CODES.R) {
                // Optional: Set navigation bar color
                window.setNavigationBarColor(Color.BLACK);
            }
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "Failed to hide status bar dialog", e);
        }
    }

    /**
     * Hides status and navigation bars in player mode.
     * <p>
     * Implements immersive mode with transient system bars that appear on swipe.
     * </p>
     *
     * @param mContext The activity to configure for player mode
     */
    public static void playerHideStatusBar(Activity mContext) {
        if (mContext == null) {
            ApplicationUtil.log(TAG, "Activity context is null player Hide StatusBar");
            return;
        }
        try {
            Window window = mContext.getWindow();
            View decorView = window.getDecorView();

            // Allow content to extend behind system bars
            WindowCompat.setDecorFitsSystemWindows(window, false);

            // Use compatible insets controller
            WindowInsetsControllerCompat controller = new WindowInsetsControllerCompat(window, decorView);

            // Hide status and navigation bars
            controller.hide(WindowInsetsCompat.Type.statusBars() | WindowInsetsCompat.Type.navigationBars());

            // Enable immersive sticky behavior (swipe to temporarily show bars)
            controller.setSystemBarsBehavior(
                    WindowInsetsControllerCompat.BEHAVIOR_SHOW_TRANSIENT_BARS_BY_SWIPE
            );
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "Failed to hide player status bar", e);
        }
    }

    /**
     * Checks if developer mode is enabled on the device.
     * <p>
     * Note: Requires API level 31 (Android S) or above to check accurately.
     * </p>
     *
     * @param activity The activity context
     * @return true if developer mode is enabled, false otherwise or if not supported
     */
    public static boolean isDeveloperModeEnabled(Activity activity) {
        if (activity == null) {
            ApplicationUtil.log(TAG, "Activity context is null isDeveloperModeEnabled");
            return false;
        }
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            try {
                return Settings.Global.getInt(activity.getContentResolver(),
                        Settings.Global.DEVELOPMENT_SETTINGS_ENABLED, 0) == 1;
            } catch (SecurityException e) {
                ApplicationUtil.log(TAG, "No permission to access developer settings");
                return false;
            }
        }
        ApplicationUtil.log(TAG, "No permission to access developer settings");
        return false;
    }

    /**
     * Checks if a VPN connection is currently active.
     *
     * @param context The context
     * @return true if VPN is active, false otherwise or if not supported
     */
    public static boolean isVPNActive(Context context) {
        if (context == null) {
            ApplicationUtil.log(TAG, "Activity context is null isVPNActive");
            return false;
        }
        ConnectivityManager connectivityManager = (ConnectivityManager) context.getSystemService(Context.CONNECTIVITY_SERVICE);
        if (connectivityManager == null) {
            return false;
        }
        Network activeNetwork = connectivityManager.getActiveNetwork();
        if (activeNetwork == null) {
            return false;
        }
        NetworkCapabilities capabilities = connectivityManager.getNetworkCapabilities(activeNetwork);
        return capabilities != null && capabilities.hasTransport(NetworkCapabilities.TRANSPORT_VPN);
    }
}
