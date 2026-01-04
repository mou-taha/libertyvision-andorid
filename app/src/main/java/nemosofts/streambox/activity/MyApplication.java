package nemosofts.streambox.activity;

import android.content.Context;
import android.os.StrictMode;

import androidx.multidex.MultiDex;
import androidx.nemosofts.Application;
import androidx.nemosofts.optimized.PicassoOptimized;

import com.google.firebase.analytics.FirebaseAnalytics;
import com.onesignal.OneSignal;
import com.squareup.picasso.Picasso;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.atomic.AtomicBoolean;

import nemosofts.streambox.BuildConfig;
import nemosofts.streambox.R;
import nemosofts.streambox.utils.ApplicationUtil;
import nemosofts.streambox.utils.helper.DBHelper;
import nemosofts.streambox.utils.helper.Helper;

public class MyApplication extends Application {

    private final AtomicBoolean databaseInitScheduled = new AtomicBoolean(false);
    private final AtomicBoolean databaseReady = new AtomicBoolean(false);

    @Override
    public void onCreate() {
        super.onCreate();

        // Analytics Initialization
        FirebaseAnalytics.getInstance(getApplicationContext());

        StrictMode.VmPolicy.Builder builder = new StrictMode.VmPolicy.Builder();
        StrictMode.setVmPolicy(builder.build());

        initDatabase();

        // OneSignal Initialization
        OneSignal.initWithContext(this, getString(R.string.onesignal_app_id));

        initPicasso();

        new Helper(getApplicationContext()).initializeAds();
    }

    private void initDatabase() {
        if (databaseReady.get() || databaseInitScheduled.get()) {
            return;
        }

        if (!databaseInitScheduled.compareAndSet(false, true)) {
            return;
        }

        final ExecutorService executor = Executors.newSingleThreadExecutor();
        executor.execute(() -> {
            try {
                DBHelper dbHelper = new DBHelper(getApplicationContext());
                dbHelper.getWritableDatabase();
                dbHelper.close();
                databaseReady.set(true);
            } catch (Exception initializationError) {
                databaseInitScheduled.set(false);
                ApplicationUtil.log("MyApplication", "Database initialization failed", initializationError);
            } finally {
                executor.shutdown();
            }
        });
    }

    private void initPicasso() {
        try {
            Picasso.setSingletonInstance(PicassoOptimized.create(getApplicationContext()));
        } catch (IllegalStateException alreadySet) {
            ApplicationUtil.log("MyApplication", "Picasso already initialized, using existing instance", alreadySet);
        }
    }

    @Override
    public String setProductID() {
        return "52621164";
    }

    @Override
    public String setApplicationID() {
        return BuildConfig.APPLICATION_ID;
    }

    @Override
    protected void attachBaseContext(Context base) {
        super.attachBaseContext(base);
        MultiDex.install(this);
    }
}