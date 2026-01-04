package nemosofts.streambox.utils;

import android.Manifest;
import android.content.ActivityNotFoundException;
import android.content.BroadcastReceiver;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.os.Build;

import androidx.annotation.NonNull;
import androidx.core.content.ContextCompat;

import nemosofts.streambox.activity.LauncherActivity;
import nemosofts.streambox.utils.helper.SPHelper;

public class BootReceiver extends BroadcastReceiver {

    private static final String TAG = "BootReceiver";

    @Override
    public void onReceive(Context context, Intent intent) {
        if (intent == null || intent.getAction() == null) {
            ApplicationUtil.log(TAG, "Received null intent or action");
            return;
        }

        if (!isBootAction(context, intent.getAction())) {
            return;
        }

        handleBootEvent(context);
    }

    private void handleBootEvent(Context context) {
        try {
            SPHelper spHelper = new SPHelper(context);
            if (!spHelper.getIsAutoStart()) {
                ApplicationUtil.log(TAG, "Auto-start disabled in preferences");
                return;
            }

            launchMainActivity(context);
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "Error during boot handling", e);
        }
    }

    private boolean isBootAction(Context context, @NonNull String action) {
        // Common actions for all API levels
        boolean isStandardBoot = action.equals(Intent.ACTION_BOOT_COMPLETED);
        boolean isQuickBoot = action.equals("android.intent.action.QUICKBOOT_POWERON")
                || action.equals("com.htc.intent.action.QUICKBOOT_POWERON");

        // For API 24+ (Nougat) devices
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
            boolean isLockedBoot = action.equals(Intent.ACTION_LOCKED_BOOT_COMPLETED)
                    && hasBootPermission(context);
            return isStandardBoot || isLockedBoot || isQuickBoot;
        }

        // For older devices
        return (isStandardBoot && hasBootPermission(context)) || isQuickBoot;
    }

    private boolean hasBootPermission(@NonNull Context context) {
        try {
            return ContextCompat.checkSelfPermission(
                    context, Manifest.permission.RECEIVE_BOOT_COMPLETED
            ) == PackageManager.PERMISSION_GRANTED;
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "Permission check failed", e);
            return false;
        }
    }

    private void launchMainActivity(Context context) {
        try {
            Intent launchIntent = createLaunchIntent(context);
            context.startActivity(launchIntent);
            ApplicationUtil.log(TAG, "Activity launched successfully");
        } catch (SecurityException e) {
            ApplicationUtil.log(TAG, "Security exception - may need to check permissions", e);
        } catch (ActivityNotFoundException e) {
            ApplicationUtil.log(TAG, "Activity not found - check your manifest", e);
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "Unexpected error launching activity", e);
        }
    }

    @NonNull
    private Intent createLaunchIntent(Context context) {
        Intent intent = new Intent(context, LauncherActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK |
                Intent.FLAG_ACTIVITY_RESET_TASK_IF_NEEDED);

        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TASK);

            // For Android 12+ compatibility
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
                intent.addFlags(Intent.FLAG_ACTIVITY_NEW_DOCUMENT);
            }
        }
        return intent;
    }
}