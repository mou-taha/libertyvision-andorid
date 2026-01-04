package nemosofts.streambox.activity;

import android.content.ActivityNotFoundException;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.net.Uri;
import android.os.Bundle;
import android.widget.TextView;

import androidx.activity.EdgeToEdge;
import androidx.activity.OnBackPressedCallback;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.nemosofts.AppCompatActivity;

import nemosofts.streambox.R;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.utils.IfSupported;
import nemosofts.streambox.utils.OpenVpnConnector;
import nemosofts.streambox.utils.helper.ThemeHelper;

public class OpenVPNHelpActivity extends AppCompatActivity {

    private String from = "";

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

        Intent intent = getIntent();
        if (intent.hasExtra("from")){
            from = getIntent().getStringExtra("from");
        }

        initializeUI();
        setupBackPressHandler();
    }

    private void initializeUI() {
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ThemeHelper.getThemeBackgroundRes(this));

        TextView title = findViewById(R.id.tv_help_title);
        title.setText(R.string.openvpn_help_title);

        findViewById(R.id.btn_openvpn_help_open_setup).setOnClickListener(v -> openSetup());
        findViewById(R.id.btn_openvpn_help_get_app).setOnClickListener(v -> openStore());
    }

    private void setupBackPressHandler() {
        OnBackPressedCallback callback = new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                if (from.equals("app")) {
                    finish();
                } else {
                    DialogUtil.exitDialog(OpenVPNHelpActivity.this);
                }
            }
        };
        getOnBackPressedDispatcher().addCallback(this, callback);
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_open_vpn_help;
    }

    private void openSetup() {
        if (from.equals("app")) {
            finish();
        } else {
            Intent intent = new Intent(this, OpenVPNActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            startActivity(intent);
            finish();
        }
    }

    private void openStore() {
        try {
            Intent intent = new Intent(Intent.ACTION_VIEW,
                    Uri.parse("market://details?id=" + OpenVpnConnector.OPENVPN_PACKAGE));
            intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            startActivity(intent);
        } catch (ActivityNotFoundException e) {
            Intent webIntent = new Intent(Intent.ACTION_VIEW,
                    Uri.parse("https://play.google.com/store/apps/details?id=" + OpenVpnConnector.OPENVPN_PACKAGE));
            webIntent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            startActivity(webIntent);
        }
    }
}