package nemosofts.streambox.utils;

import android.app.Activity;
import android.content.ActivityNotFoundException;
import android.content.Context;
import android.content.Intent;
import android.content.pm.PackageManager;
import android.net.Uri;
import android.text.TextUtils;

import androidx.annotation.NonNull;
import androidx.core.content.FileProvider;
import androidx.nemosofts.material.Toasty;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.nio.charset.StandardCharsets;

import nemosofts.streambox.BuildConfig;
import nemosofts.streambox.R;

public final class OpenVpnConnector {

    private static final String TAG = "OpenVpnConnector";
    private static final String MIME_TYPE_OVPN = "application/x-openvpn-profile";
    public static final String OPENVPN_PACKAGE = "net.openvpn.openvpn";
    private static final String OVPN_CACHE_DIR = "openvpn";

    private OpenVpnConnector() {
        throw new IllegalStateException("Utility class");
    }

    public static boolean connect(@NonNull Activity activity,
                                  @NonNull String ovpnConfig,
                                  @NonNull String username,
                                  @NonNull String password) {
        if (TextUtils.isEmpty(ovpnConfig) || TextUtils.isEmpty(username) || TextUtils.isEmpty(password)) {
            Toasty.makeText(activity,true,
                    activity.getString(R.string.err_vpn_config_missing), Toasty.ERROR);
            return false;
        }

        try {
            if (!isOpenVpnInstalled(activity)) {
                promptOpenVpnInstall(activity);
                return false;
            }

            Uri configUri = persistConfig(activity, ovpnConfig);
            Intent openVpnIntent = new Intent(Intent.ACTION_VIEW);
            openVpnIntent.setDataAndType(configUri, MIME_TYPE_OVPN);
            openVpnIntent.putExtra(Intent.EXTRA_TITLE, activity.getString(R.string.openvpn_connect));
            openVpnIntent.addFlags(Intent.FLAG_GRANT_READ_URI_PERMISSION);
            openVpnIntent.setPackage(OPENVPN_PACKAGE);
            PackageManager packageManager = activity.getPackageManager();
            if (openVpnIntent.resolveActivity(packageManager) != null) {
                activity.startActivity(openVpnIntent);
                Toasty.makeText(activity,true,
                        activity.getString(R.string.msg_vpn_hand_off), Toasty.SUCCESS);
                ApplicationUtil.log(TAG, "VPN intent sent to OpenVPN Connect.");
                return true;
            } else {
                promptOpenVpnInstall(activity);
            }
        } catch (IOException e) {
            ApplicationUtil.log(TAG, "Failed to cache VPN config", e);
            Toasty.makeText(activity,true,
                    activity.getString(R.string.err_creating_vpn_file), Toasty.ERROR);
        } catch (ActivityNotFoundException e) {
            ApplicationUtil.log(TAG, "OpenVPN Connect activity missing", e);
            Toasty.makeText(activity,true,
                    activity.getString(R.string.err_start_vpn), Toasty.ERROR);
            promptOpenVpnInstall(activity);
        }
        return false;
    }

    private static boolean isOpenVpnInstalled(@NonNull Context context) {
        try {
            PackageManager packageManager = context.getPackageManager();
            packageManager.getPackageInfo(OPENVPN_PACKAGE, 0);
            return true;
        } catch (PackageManager.NameNotFoundException e) {
            return false;
        }
    }

    private static void promptOpenVpnInstall(@NonNull Activity activity) {
        Toasty.makeText(activity,true,
                activity.getString(R.string.err_openvpn_missing), Toasty.ERROR);
        Intent intent = new Intent(Intent.ACTION_VIEW,
                Uri.parse("market://details?id=" + OPENVPN_PACKAGE));
        try {
            activity.startActivity(intent);
        } catch (ActivityNotFoundException e) {
            Intent webIntent = new Intent(Intent.ACTION_VIEW,
                    Uri.parse("https://play.google.com/store/apps/details?id=" + OPENVPN_PACKAGE));
            activity.startActivity(webIntent);
        }
    }

    @NonNull
    private static Uri persistConfig(@NonNull Context context,
                                     @NonNull String ovpnConfig) throws IOException {
        File dir = new File(context.getCacheDir(), OVPN_CACHE_DIR);
        if (!dir.exists() && !dir.mkdirs()) {
            throw new IOException("Unable to create cache directory for ovpn config");
        }
        File profileFile = new File(dir, "profile.ovpn");
        try (FileOutputStream outputStream = new FileOutputStream(profileFile, false)) {
            outputStream.write(ovpnConfig.getBytes(StandardCharsets.UTF_8));
            outputStream.flush();
        }
        return FileProvider.getUriForFile(
                context,
                BuildConfig.APPLICATION_ID + ".fileprovider",
                profileFile
        );
    }
}
