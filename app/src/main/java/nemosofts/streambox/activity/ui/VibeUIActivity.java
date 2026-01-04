package nemosofts.streambox.activity.ui;

import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.widget.ImageView;

import androidx.activity.EdgeToEdge;
import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.fragment.app.Fragment;
import androidx.fragment.app.FragmentManager;
import androidx.fragment.app.FragmentPagerAdapter;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.IconTextView;

import androidx.nemosofts.utils.DeviceUtils;
import androidx.viewpager.widget.ViewPager;

import com.onesignal.Continue;
import com.onesignal.OneSignal;

import nemosofts.streambox.R;
import nemosofts.streambox.activity.NotificationsActivity;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.fragment.FragmentLive;
import nemosofts.streambox.fragment.FragmentMovie;
import nemosofts.streambox.fragment.FragmentSeries;
import nemosofts.streambox.utils.ApplicationUtil;
import nemosofts.streambox.utils.IfSupported;
import nemosofts.streambox.utils.helper.ThemeHelper;

public class VibeUIActivity extends AppCompatActivity {

    private ViewPager mViewPager;
    private IconTextView menuMovies;
    private IconTextView menuLive;
    private IconTextView menuSeries;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        super.onCreate(savedInstanceState);
        EdgeToEdge.enable(this);
        ViewCompat.setOnApplyWindowInsetsListener(findViewById(R.id.theme_bg), (v, insets) -> {
            Insets systemInsets = insets.getInsets(WindowInsetsCompat.Type.systemBars());
            v.setPadding(systemInsets.left, systemInsets.top, systemInsets.right, systemInsets.bottom);
            return insets;
        });

        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ThemeHelper.getThemeBackgroundRes(this));

        ImageView imageView = findViewById(R.id.iv_wifi);
        ApplicationUtil.setWifiIcon(imageView, this);

        menuMovies = findViewById(R.id.btn_menu_movies);
        menuLive = findViewById(R.id.btn_menu_live);
        menuSeries = findViewById(R.id.btn_menu_series);

        setupViewPager();
        setOnClickListener();

        // requestPermission will show the native Android notification permission prompt.
        // NOTE: It's recommended to use a OneSignal In-App Message to prompt instead.
        OneSignal.getNotifications().requestPermission(false, Continue.none());

        setupBackPressHandler();
        if (DeviceUtils.isTvBox(this)){
            findViewById(R.id.btn_menu_movies).requestFocus();
        }
    }

    private void setOnClickListener() {
        menuMovies.setOnClickListener(v -> mViewPager.setCurrentItem(0));
        menuLive.setOnClickListener(v ->{
            mViewPager.setCurrentItem(1);
            FragmentLive liveFragment = (FragmentLive) getSupportFragmentManager()
                    .findFragmentByTag("android:switcher:" + R.id.container + ":" + mViewPager.getCurrentItem());
            if (liveFragment != null) {
                liveFragment.loadInitialData();
            }
        });
        menuSeries.setOnClickListener(v -> {
            mViewPager.setCurrentItem(2);
            FragmentSeries seriesFragment = (FragmentSeries) getSupportFragmentManager()
                    .findFragmentByTag("android:switcher:" + R.id.container + ":" + mViewPager.getCurrentItem());
            if (seriesFragment != null) {
                seriesFragment.loadInitialData();
            }
        });

        findViewById(R.id.iv_profile).setOnClickListener(v ->
                DialogUtil.menuDialog(VibeUIActivity.this, () -> {
                    mViewPager.setCurrentItem(0);
                    recreate();
                })
        );
        findViewById(R.id.iv_notifications).setOnClickListener(v ->
                startActivity(new Intent(VibeUIActivity.this, NotificationsActivity.class))
        );
    }

    private void setupViewPager() {
        mViewPager = findViewById(R.id.container);
        mViewPager.setOffscreenPageLimit(4);
        mViewPager.setAdapter(new SectionsPagerAdapter(getSupportFragmentManager()));
        mViewPager.addOnPageChangeListener(new ViewPager.OnPageChangeListener() {
            @Override
            public void onPageScrolled(int position, float positionOffset, int positionOffsetPixels) {
                // this method is empty
            }

            @Override
            public void onPageSelected(int position) {
                onMenuSelected(position);
            }

            @Override
            public void onPageScrollStateChanged(int state) {
                // this method is empty
            }
        });
        onMenuSelected(0);
    }

    private void onMenuSelected(int position) {
        // Reset all to default
        menuMovies.setBackgroundResource(R.drawable.focused_menu);
        menuLive.setBackgroundResource(R.drawable.focused_menu);
        menuSeries.setBackgroundResource(R.drawable.focused_menu);

        // Apply active state
        switch (position) {
            case 0:
                menuMovies.setBackgroundResource(R.drawable.focused_menu_active);
                break;
            case 1:
                menuLive.setBackgroundResource(R.drawable.focused_menu_active);
                break;
            case 2:
                menuSeries.setBackgroundResource(R.drawable.focused_menu_active);
                break;
            default:
                break;
        }
    }

    public static class SectionsPagerAdapter extends FragmentPagerAdapter {

        SectionsPagerAdapter(FragmentManager fm) {
            super(fm);
        }

        @NonNull
        @Override
        public Fragment getItem(int position) {
            return switch (position) {
                case 0 -> FragmentMovie.newInstance();
                case 1 -> FragmentLive.newInstance();
                default -> FragmentSeries.newInstance();
            };
        }

        @Override
        public int getCount() {
            return 3;
        }
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_themes_vibe_ui;
    }

    @Override
    public void onResume() {
        if (Boolean.TRUE.equals(Callback.getIsRecreate())) {
            Callback.setIsRecreate(false);
            recreate();
        }
        super.onResume();
    }

    private void setupBackPressHandler() {
        OnBackPressedCallback callback = new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                DialogUtil.exitDialog(VibeUIActivity.this);
            }
        };
        getOnBackPressedDispatcher().addCallback(this, callback);
    }
}