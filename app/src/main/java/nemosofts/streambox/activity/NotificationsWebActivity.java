package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.pm.ActivityInfo;
import android.graphics.Color;
import android.os.Bundle;
import android.util.Log;
import android.view.KeyEvent;
import android.view.View;
import android.webkit.WebView;

import androidx.activity.EdgeToEdge;
import androidx.annotation.NonNull;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.utils.DeviceUtils;

import nemosofts.streambox.R;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.utils.IfSupported;
import nemosofts.streambox.utils.helper.SPHelper;
import nemosofts.streambox.utils.helper.ThemeHelper;

public class NotificationsWebActivity extends AppCompatActivity {

    @SuppressLint("SetJavaScriptEnabled")
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

        initializeUI();

        WebView webView = findViewById(R.id.web);
        webView.getSettings().setJavaScriptEnabled(true);

        webView.clearCache(true);
        webView.clearHistory();

        webView.setScrollbarFadingEnabled(true);
        webView.setBackgroundColor(Color.TRANSPARENT);

        // Load the HTML into the WebView
        webView.loadDataWithBaseURL("", getHtmlString(), "text/html", "utf-8", null);
    }

    private void initializeUI() {
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ThemeHelper.getThemeBackgroundRes(this));

        findViewById(R.id.iv_back_page).setOnClickListener(view -> finish());
        if (DeviceUtils.isTvBox(this)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }
    }

    @NonNull
    private String getHtmlString() {
        String htmlText = "";
        try {
            htmlText = Callback.getArrayListNotify().get(Callback.getPosNotify()).getDescription();
        } catch (Exception e) {
            Log.e("NotificationsWebActivity", "getHtmlString: ", e);
        }
        return getHtmlString(htmlText);
    }

    @NonNull
    private String getHtmlString(String htmlCode) {
        String htmlString;
        if (Boolean.FALSE.equals(new SPHelper(this).getIsRTL())) {
            htmlString = "<html><head><style>body{color: white; font-size: 15px;}</style></head><body>"
                    + htmlCode
                    + "</body></html>";
        } else {
            htmlString = "<htmldir=\"rtl\" lang=\"\"><head><style>body{color: white; font-size: 15px;}</style></head><body>"
                    + htmlCode
                    + "</body></html>";
        }
        return htmlString;
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_notifications_web;
    }

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