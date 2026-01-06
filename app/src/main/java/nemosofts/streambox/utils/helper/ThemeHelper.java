package nemosofts.streambox.utils.helper;

import android.app.Activity;
import android.content.Intent;
import android.view.View;

import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.theme.ThemeEngine;

import nemosofts.streambox.R;
import nemosofts.streambox.activity.PlaylistActivity;
import nemosofts.streambox.activity.SelectPlayerActivity;
import nemosofts.streambox.activity.SingleStreamActivity;
import nemosofts.streambox.activity.ui.BlackPantherActivity;
import nemosofts.streambox.activity.ui.ChristmasActivity;
import nemosofts.streambox.activity.ui.GlossyActivity;
import nemosofts.streambox.activity.ui.HalloweenActivity;
import nemosofts.streambox.activity.ui.MovieUiActivity;
import nemosofts.streambox.activity.ui.OneUIActivity;
import nemosofts.streambox.activity.ui.RamadanActivity;
import nemosofts.streambox.activity.ui.SportsActivity;
import nemosofts.streambox.activity.ui.VibeUIActivity;
import nemosofts.streambox.activity.ui.VUIActivity;
import nemosofts.streambox.callback.Callback;


public class ThemeHelper {

    // Constants for special theme types
    private static final int THEME_GLOSSY = 2;
    private static final int THEME_DARK_PANTHER = 3;
    private static final int THEME_MOVIE_UI = 4;
    private static final int THEME_VUI = 5;
    private static final int THEME_CHRISTMAS = 6;
    private static final int THEME_HALLOWEEN = 7;
    private static final int THEME_RAMADAN = 8;
    private static final int THEME_SPORTS = 9;
    private static final int THEME_TV_UI = 10;

    // Constants for page theme types
    private static final int THEME_PAGE_DARK = 0;
    private static final int THEME_PAGE_CLASSIC = 1;
    private static final int THEME_PAGE_GREY = 2;
    private static final int THEME_PAGE_BLUE = 3;

    private ThemeHelper() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Applies the selected theme to the specified background view.
     * If the theme is a special theme (GLOSSY or DARK_PANTHER), it applies the corresponding background.
     * Otherwise, it applies the appropriate page theme.
     *
     * @param activity      The current activity context
     * @param spHelper      The shared preferences helper containing theme settings
     * @param themeBgView   The view to which the theme background should be applied
     * @throws NullPointerException if spHelper is null
     */
    public static void applyTheme(Activity activity, @NonNull SPHelper spHelper, View themeBgView) {
        int theme = spHelper.getIsTheme();
        if (theme == THEME_GLOSSY) {
            themeBgView.setBackgroundResource(R.drawable.bg_ui_glossy);
        } else if (theme == THEME_DARK_PANTHER) {
            themeBgView.setBackgroundResource(R.drawable.bg_dark_panther);
        } else {
            applyThemePage(activity, themeBgView);
        }
    }

    /**
     * Applies the appropriate page theme to the specified background view.
     * The theme is determined by the current ThemeEngine settings.
     *
     * @param activity      The current activity context
     * @param themeBgView   The view to which the theme background should be applied
     */
    private static void applyThemePage(Activity activity, View themeBgView) {
        int themePage = new ThemeEngine(activity).getThemePage();
        switch (themePage) {
            case THEME_PAGE_CLASSIC:
                themeBgView.setBackgroundResource(R.drawable.bg_classic);
                break;
            case THEME_PAGE_GREY:
                themeBgView.setBackgroundResource(R.drawable.bg_grey);
                break;
            case THEME_PAGE_BLUE:
                themeBgView.setBackgroundResource(R.drawable.bg_blue);
                break;
            case THEME_PAGE_DARK:
            default:
                themeBgView.setBackgroundResource(R.drawable.bg_darkx);
                break;
        }
    }

    /**
     * Gets the resource ID of the background drawable for the current theme.
     *
     * @param activity The current activity context
     * @return The resource ID of the background drawable
     */
    public static int getThemeBackgroundRes(Activity activity) {
        int theme = new SPHelper(activity).getIsTheme();
        int themePage = new ThemeEngine(activity).getThemePage();
        return switch (theme) {
            case THEME_GLOSSY -> R.drawable.bg_ui_glossy;
            case THEME_DARK_PANTHER -> R.drawable.bg_dark_panther;
            default -> switch (themePage) {
                case THEME_PAGE_CLASSIC, THEME_PAGE_GREY, THEME_PAGE_BLUE -> R.drawable.bg_classic;
                default -> R.drawable.bg_darkx;
            };
        };
    }

    /**
     * Opens the appropriate theme activity based on the current theme settings.
     * If the activity is null or finishing, the method returns without taking any action.
     *
     * @param activity The current activity context
     */
    public static void openThemeActivity(Activity activity) {
        if (activity == null || activity.isFinishing()){
            return;
        }

        Intent intent = getThemeActivityIntent(activity, new SPHelper(activity).getIsTheme());
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        activity.startActivity(intent);
        activity.finish();
    }

    /**
     * Opens the appropriate home activity based on the user's login type and theme settings.
     * The method handles different login types (single stream, playlist, etc.) and applies theme-specific behavior.
     *
     * @param activity The current activity context
     */
    @OptIn(markerClass = UnstableApi.class)
    public static void openHomeActivity(Activity activity) {
        SPHelper spHelper = new SPHelper(activity);
        Intent intent;

        String loginType = spHelper.getLoginType();
        if (Callback.TAG_LOGIN_SINGLE_STREAM.equals(loginType)) {
            intent = new Intent(activity, SingleStreamActivity.class);
        } else if (Callback.TAG_LOGIN_PLAYLIST.equals(loginType)) {
            intent = new Intent(activity, PlaylistActivity.class);
        } else if (Callback.TAG_LOGIN_ONE_UI.equals(loginType) ||
                Callback.TAG_LOGIN_STREAM.equals(loginType)){
            intent = getThemeActivityIntent(activity, spHelper.getIsTheme());
        } else {
            intent = new Intent(activity, SelectPlayerActivity.class);
        }

        if (intent != null) {
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "");
            activity.startActivity(intent);
            activity.finish();
        }
    }

    /**
     * Creates an intent for the appropriate theme activity based on the specified theme.
     *
     * @param activity The current activity context
     * @param theme    The theme type to determine which activity to launch
     * @return An Intent for the appropriate theme activity, or null if activity is null
     */
    private static Intent getThemeActivityIntent(Activity activity, int theme) {
        if (activity == null) {
            return null;
        }
        return switch (theme) {
            case THEME_GLOSSY -> new Intent(activity, GlossyActivity.class);
            case THEME_DARK_PANTHER -> new Intent(activity, BlackPantherActivity.class);
            case THEME_MOVIE_UI -> new Intent(activity, MovieUiActivity.class);
            case THEME_VUI -> new Intent(activity, VUIActivity.class);
            case THEME_CHRISTMAS -> new Intent(activity, ChristmasActivity.class);
            case THEME_HALLOWEEN -> new Intent(activity, HalloweenActivity.class);
            case THEME_RAMADAN -> new Intent(activity, RamadanActivity.class);
            case THEME_SPORTS -> new Intent(activity, SportsActivity.class);
            case THEME_TV_UI -> new Intent(activity, VibeUIActivity.class);
            default -> new Intent(activity, OneUIActivity.class);
        };
    }
}