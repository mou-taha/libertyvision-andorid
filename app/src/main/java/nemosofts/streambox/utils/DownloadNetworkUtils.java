package nemosofts.streambox.utils;

import android.content.Context;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.nemosofts.utils.NetworkUtils;

import nemosofts.streambox.utils.helper.SPHelper;

public final class DownloadNetworkUtils {

    private DownloadNetworkUtils() {
        throw new IllegalStateException("Utility class");
    }

    @NonNull
    public static Boolean canDownload(@NonNull Context context, @Nullable SPHelper helper) {
        SPHelper spHelper = helper != null ? helper : new SPHelper(context);
        boolean allowWifi = spHelper.isDownloadWifiAllowed();
        boolean allowMobile = spHelper.isDownloadMobileAllowed();

        if (!allowWifi && !allowMobile) {
            return false;
        }

        if (!NetworkUtils.isConnected(context)) {
            return false;
        }

        boolean wifiActive = isWifiConnected(context);
        boolean mobileActive = isCellularConnected(context);

        if (allowWifi && wifiActive) {
            return true;
        }

        return allowMobile && mobileActive;
    }

    public static boolean isWifiConnected(@NonNull Context context) {
        return NetworkUtils.isConnectedWifi(context) || NetworkUtils.isConnectedEthernet(context);
    }

    public static boolean isCellularConnected(@NonNull Context context) {
        return NetworkUtils.isConnectedMobile(context);
    }
}