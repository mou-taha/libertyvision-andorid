package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.View;
import android.widget.TextView;

import androidx.activity.EdgeToEdge;
import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.Toasty;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterOpenVpnProfile;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.item.ItemOpenVpnProfile;
import nemosofts.streambox.utils.IfSupported;
import nemosofts.streambox.utils.OpenVpnConnector;
import nemosofts.streambox.utils.helper.OpenVpnProfileHelper;
import nemosofts.streambox.utils.helper.ThemeHelper;

public class OpenVPNProfileActivity extends AppCompatActivity {

    private String from = "";
    private RecyclerView rvProfiles;
    private TextView tvEmpty;
    private AdapterOpenVpnProfile adapter;
    private final List<ItemOpenVpnProfile> profiles = new ArrayList<>();
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

        initializeUI();
        setupRecyclerView();
        setupAdapter();
        setupBackPressHandler();
        findViewById(R.id.btn_add_profile).setOnClickListener(v -> openCreateProfile());
        loadProfiles();
    }

    private void initializeUI() {
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ThemeHelper.getThemeBackgroundRes(this));
        profileStore = new OpenVpnProfileHelper(this);
        rvProfiles = findViewById(R.id.rv_profiles);
        tvEmpty = findViewById(R.id.tv_empty_state);
    }

    private void setupRecyclerView() {
        rvProfiles.setLayoutManager(new LinearLayoutManager(this));
    }

    private void setupAdapter() {
        adapter = new AdapterOpenVpnProfile(profiles, new AdapterOpenVpnProfile.ProfileListener() {
            @Override
            public void onProfileClicked(@NonNull ItemOpenVpnProfile profile) {
                if (profile.getConfig().isEmpty()) {
                    Toasty.makeText(OpenVPNProfileActivity.this,true,
                            getString(R.string.err_vpn_config_missing), Toasty.ERROR);
                    return;
                }
                String username = profile.getUsername();
                String password = profile.getPassword();
                if (TextUtils.isEmpty(username) || TextUtils.isEmpty(password)) {
                    Toasty.makeText(OpenVPNProfileActivity.this,true,
                            getString(R.string.err_vpn_profile_incomplete), Toasty.ERROR);
                    return;
                }
                OpenVpnConnector.connect(OpenVPNProfileActivity.this,
                        profile.getConfig(),
                        username,
                        password);
            }

            @Override
            public void onProfileLongClick(@NonNull ItemOpenVpnProfile profile) {
                DialogUtil.deleteDialog(OpenVPNProfileActivity.this, () -> {
                    if (profileStore != null) {
                        profileStore.deleteProfile(profile.getId());
                        loadProfiles();
                    }
                });
            }
        });
        rvProfiles.setAdapter(adapter);
    }

    private void setupBackPressHandler() {
        OnBackPressedCallback callback = new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                if (from.equals("app")) {
                    finish();
                } else {
                    DialogUtil.exitDialog(OpenVPNProfileActivity.this);
                }
            }
        };
        getOnBackPressedDispatcher().addCallback(this, callback);
    }

    private void openCreateProfile() {
        Intent intent = new Intent(this, OpenVPNActivity.class);
        if (from.equals("app")) {
            intent.putExtra("from", "app");
        } else {
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "");
        }
        startActivity(intent);
        if (!from.equals("app")) {
            finish();
        }
    }

    @SuppressLint("NotifyDataSetChanged")
    private void loadProfiles() {
        if (profileStore == null) {
            return;
        }
        profiles.clear();
        profiles.addAll(profileStore.getProfiles());
        adapter.notifyDataSetChanged();
        tvEmpty.setVisibility(profiles.isEmpty() ? View.VISIBLE : View.GONE);
    }

    @Override
    protected void onResume() {
        super.onResume();
        loadProfiles();
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_open_vpn_profiles;
    }
}