package nemosofts.streambox.activity;

import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.net.Uri;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.text.TextUtils;
import android.view.View;
import android.widget.EditText;
import android.widget.TextView;

import androidx.activity.EdgeToEdge;
import androidx.activity.OnBackPressedCallback;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.Toasty;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.net.HttpURLConnection;
import java.net.URL;

import nemosofts.streambox.R;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.item.ItemOpenVpnProfile;
import nemosofts.streambox.utils.ApplicationUtil;
import nemosofts.streambox.utils.OpenVpnConnector;
import nemosofts.streambox.utils.helper.OpenVpnProfileHelper;

public class OpenVPNActivity extends AppCompatActivity {

    private static final String TAG = "OpenVPNActivity";
    private String from = "";
    private EditText etUserName;
    private EditText etLoginPassword;
    private EditText urlEditText;
    private TextView btnBrowse;
    private String oVpnConfig = "";
    private OpenVpnProfileHelper profileStore;

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

        etUserName = findViewById(R.id.et_user_name);
        etLoginPassword = findViewById(R.id.et_login_password);
        urlEditText = findViewById(R.id.et_url);
        profileStore = new OpenVpnProfileHelper(this);

        findViewById(R.id.rd_1).setOnClickListener(view -> setIsFile(true));
        findViewById(R.id.rd_2).setOnClickListener(view -> setIsFile(false));
        findViewById(R.id.ll_btn_connected).setOnClickListener(v -> attemptConnected());

        btnBrowse = findViewById(R.id.btn_browse);
        btnBrowse.setOnClickListener(v -> pickOvpnFile());
        findViewById(R.id.btn_openvpn_profiles).setOnClickListener(v -> openProfilesActivity());
        findViewById(R.id.btn_openvpn_help).setOnClickListener(v -> openHelpActivity());
        findViewById(R.id.btn_list_users).setOnClickListener(view -> openUsersListActivity());
        if (from.equals("app")) {
            findViewById(R.id.btn_list_users).setVisibility(View.GONE);
        }
        setupBackPressHandler();
    }

    private void setIsFile(boolean file) {
        findViewById(R.id.ll_browse).setVisibility(file ? View.VISIBLE : View.GONE);
        findViewById(R.id.ll_url).setVisibility(file ? View.GONE : View.VISIBLE);
    }

    private final ActivityResultLauncher<Intent> pickOvpnFileLauncher =
            registerForActivityResult(new ActivityResultContracts.StartActivityForResult(), result -> {
                if (result.getResultCode() == RESULT_OK && result.getData() != null) {
                    Uri fileUri = result.getData().getData();
                    try {
                        oVpnConfig = readOvpnFile(fileUri);
                        btnBrowse.setBackgroundResource(R.drawable.focused_btn_success);
                        new Handler(Looper.getMainLooper()).postDelayed(() ->
                                Toasty.makeText(OpenVPNActivity.this,
                                        getString(R.string.added_success), Toasty.SUCCESS), 0
                        );
                    } catch (Exception e) {
                        btnBrowse.setBackgroundResource(R.drawable.focused_btn_danger);
                        new Handler(Looper.getMainLooper()).postDelayed(() ->
                                Toasty.makeText(OpenVPNActivity.this,
                                        getString(R.string.err_file_invalid), Toasty.ERROR), 0
                        );
                    }
                }
            }
    );

    private void attemptConnected() {
        etUserName.setError(null);
        etLoginPassword.setError(null);

        String userName = etUserName.getText().toString();
        String password = etLoginPassword.getText().toString();
        String url = urlEditText.getText().toString();

        boolean cancel = false;
        View focusView = null;

        // Validate inputs
        if (isInputInvalid(password, userName)) {
            cancel = true;
            focusView = getFocusView();
        }

        if (urlEditText.getVisibility() == View.VISIBLE && TextUtils.isEmpty(url)) {
            urlEditText.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_cannot_empty)));
            cancel = true;
            if (etLoginPassword.getError() != null) {
                focusView = etLoginPassword;
            }
        }

        // Handle login or focus correction
        if (cancel && focusView != null) {
            focusView.requestFocus();
        } else {
            if (urlEditText.getVisibility() == View.VISIBLE){
                fetchOvpnFromUrl(url, userName, password);
            } else {
                if (!oVpnConfig.isEmpty()) {
                    saveProfile(userName, password, oVpnConfig, getString(R.string.ovpn_file));
                    startVpn(oVpnConfig, userName, password);
                } else {
                    Toasty.makeText(this,true, getString(R.string.err_vpn_config_missing), Toasty.ERROR);
                }
            }
        }
    }

    private boolean isInputInvalid(String password, String userName) {
        boolean isInvalid = false;
        if (TextUtils.isEmpty(password)) {
            etLoginPassword.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_cannot_empty)));
            isInvalid = true;
        } else if (password.endsWith(" ")) {
            etLoginPassword.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_pass_end_space)));
            isInvalid = true;
        }

        if (TextUtils.isEmpty(userName)) {
            etUserName.setError(ApplicationUtil.setErrorMsg(getString(R.string.err_cannot_empty)));
            isInvalid = true;
        }
        return isInvalid;
    }

    @Nullable
    private View getFocusView() {
        if (etLoginPassword.getError() != null) {
            return etLoginPassword;
        } else if (etUserName.getError() != null) {
            return etUserName;
        }
        return null;
    }

    private void pickOvpnFile() {
        btnBrowse.setBackgroundResource(R.drawable.focused_btn_primary);
        Intent intent = new Intent(Intent.ACTION_GET_CONTENT);
        intent.setType("*/*");
        String[] mimeTypes = {"application/x-openvpn-profile", "text/plain"};
        intent.putExtra(Intent.EXTRA_MIME_TYPES, mimeTypes);
        intent.addCategory(Intent.CATEGORY_OPENABLE);
        pickOvpnFileLauncher.launch(Intent.createChooser(intent, "Select VPN Configuration File"));
    }

    private void startVpn(String ovpnConfig, String username, String password) {
        if (TextUtils.isEmpty(ovpnConfig) || TextUtils.isEmpty(username) || TextUtils.isEmpty(password)) {
            Toasty.makeText(this,true, getString(R.string.err_vpn_config_missing), Toasty.ERROR);
            return;
        }
        OpenVpnConnector.connect(this, ovpnConfig, username, password);
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_open_vpn;
    }

    private void fetchOvpnFromUrl(String ovpnUrl, String userName, String password) {
        new Thread(() -> {
            HttpURLConnection connection = null;
            try {
                URL url = new URL(ovpnUrl);
                connection = (HttpURLConnection) url.openConnection();
                connection.setConnectTimeout(15000);
                connection.setReadTimeout(20000);
                connection.setRequestMethod("GET");
                connection.connect();
                if (connection.getResponseCode() == HttpURLConnection.HTTP_OK) {
                    try (InputStream inputStream = connection.getInputStream()) {
                        oVpnConfig = readInputStream(inputStream);
                    }
                    runOnUiThread(() -> {
                        Toasty.makeText(this,true, getString(R.string.msg_vpn_config_ready), Toasty.SUCCESS);
                        saveProfile(userName, password, oVpnConfig, ovpnUrl);
                        startVpn(oVpnConfig, userName, password);
                    });
                } else {
                    int responseCode = connection.getResponseCode();
                    runOnUiThread(() -> Toasty.makeText(this,true,
                            getString(R.string.err_vpn_download_failed, responseCode),
                            Toasty.ERROR));
                }
            } catch (Exception e) {
                ApplicationUtil.log(TAG, "Error fetching OVPN file", e);
                runOnUiThread(() ->
                        Toasty.makeText(this,true, getString(R.string.err_fetching_vpn_file), Toasty.ERROR)
                );
            } finally {
                if (connection != null) {
                    connection.disconnect();
                }
            }
        }).start();
    }

    @NonNull
    private String readOvpnFile(Uri fileUri) throws IOException {
        StringBuilder config = new StringBuilder();
        try (InputStream inputStream = getContentResolver().openInputStream(fileUri);
             BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream))) {
            String line;
            while ((line = reader.readLine()) != null) {
                config.append(line).append("\n");
            }
        }
        return config.toString();
    }

    @NonNull
    private String readInputStream(InputStream inputStream) throws IOException {
        StringBuilder config = new StringBuilder();
        try (BufferedReader reader = new BufferedReader(new InputStreamReader(inputStream))) {
            String line;
            while ((line = reader.readLine()) != null) {
                config.append(line).append("\n");
            }
        }
        return config.toString();
    }

    private void saveProfile(String userName,
                             String password,
                             String config,
                             String source) {
        if (profileStore == null || TextUtils.isEmpty(config)) {
            return;
        }
        String resolvedTitle = !TextUtils.isEmpty(userName)
                ? userName
                : getString(R.string.vpn_profile_default);
        String resolvedSource = !TextUtils.isEmpty(source)
                ? source
                : getString(R.string.vpn_profile_unknown_source);
        ItemOpenVpnProfile profile = profileStore.createProfile(
                resolvedTitle,
                userName,
                password,
                config,
                resolvedSource
        );
        if (profile.getTitle().isEmpty()) {
            return;
        }
        profileStore.saveProfile(profile);
    }

    private void openProfilesActivity() {
        if (from.equals("app")) {
            finish();
        } else {
            Intent intent = new Intent(this, OpenVPNProfileActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            startActivity(intent);
            finish();
        }
    }

    private void openHelpActivity() {
        Intent intent = new Intent(this, OpenVPNHelpActivity.class);
        if (from.equals("app")) {
            intent.putExtra("from", "app");
        } else {
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        }
        startActivity(intent);
        if (!from.equals("app")) {
            finish();
        }
    }

    private void openUsersListActivity() {
        Intent intent = new Intent(this, UsersListActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
        startActivity(intent);
        finish();
    }

    private void setupBackPressHandler() {
        OnBackPressedCallback callback = new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                if (from.equals("app")) {
                    finish();
                } else {
                    DialogUtil.exitDialog(OpenVPNActivity.this);
                }
            }
        };
        getOnBackPressedDispatcher().addCallback(this, callback);
    }
}