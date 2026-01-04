package nemosofts.streambox.activity;

import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.net.Uri;
import android.os.Bundle;
import android.view.KeyEvent;

import androidx.activity.EdgeToEdge;
import androidx.activity.OnBackPressedCallback;
import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.Toasty;

import androidx.nemosofts.utils.DeviceUtils;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.ArrayList;
import java.util.Collection;

import nemosofts.streambox.BuildConfig;
import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterSelect;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.item.ItemSelect;
import nemosofts.streambox.item.ItemSelectPage;
import nemosofts.streambox.utils.IfSupported;
import nemosofts.streambox.utils.helper.DBHelper;
import nemosofts.streambox.utils.helper.SPHelper;
import nemosofts.streambox.utils.helper.ThemeHelper;

public class SelectPlayerActivity extends AppCompatActivity {

    private ArrayList<ItemSelectPage> arrayListSelectPage;
    private SPHelper spHelper;
    DBHelper dbHelper;
    boolean isTvBox;

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
        setupRecyclerView();
        setupBackPressHandler();
    }

    private void initializeUI() {
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);

        spHelper = new SPHelper(this);
        dbHelper = new DBHelper(this);
        isTvBox = DeviceUtils.isTvBox(this);

        findViewById(R.id.theme_bg).setBackgroundResource(ThemeHelper.getThemeBackgroundRes(this));

        findViewById(R.id.tv_terms).setOnClickListener(view -> {
            Intent intent = new Intent(SelectPlayerActivity.this, WebActivity.class);
            intent.putExtra("web_url", BuildConfig.BASE_URL+"terms");
            intent.putExtra("page_title", getResources().getString(R.string.terms_and_conditions));
            startActivity(intent);
        });

        arrayListSelectPage = new ArrayList<>();
        arrayListSelectPage.addAll(dbHelper.getSelectPage());
    }

    private void setupRecyclerView() {
        RecyclerView rv = findViewById(R.id.rv_list);
        GridLayoutManager grid = new GridLayoutManager(this, 2);
        grid.setSpanCount(2);
        rv.setLayoutManager(grid);
        rv.setItemAnimator(new DefaultItemAnimator());

        ArrayList<ItemSelect> arrayList = new ArrayList<>(getSelect());
        AdapterSelect adapterSelect = new AdapterSelect(arrayList, (item, position) -> select(arrayList.get(position).getTitle()));
        rv.setAdapter(adapterSelect);
        if (isTvBox){
            rv.requestFocus();
        }
    }

    @NonNull
    private Collection<? extends ItemSelect> getSelect() {
        ArrayList<ItemSelect> arrayList = new ArrayList<>();
        if (Boolean.TRUE.equals(spHelper.getIsSelect(SPHelper.TAG_SELECT_XUI))){
            arrayList.add(new ItemSelect(getString(R.string.login_with_xtream_codes), R.drawable.ic_folder_connection,false));
        }
        if (Boolean.TRUE.equals(spHelper.getIsSelect(SPHelper.TAG_SELECT_STREAM))){
            arrayList.add(new ItemSelect(getString(R.string._1_stream), R.drawable.ic_mist_line,false));
        }
        if (Boolean.TRUE.equals(spHelper.getIsSelect(SPHelper.TAG_SELECT_PLAYLIST))){
            arrayList.add(new ItemSelect(getString(R.string.m3u_playlist), R.drawable.ic_play_list,false));
        }
        if (Boolean.TRUE.equals(spHelper.getIsSelect(SPHelper.TAG_SELECT_DEVICE))){
            arrayList.add(new ItemSelect(getString(R.string.login_with_device_id), R.drawable.ic_devices,false));
        }
        if (Boolean.TRUE.equals(spHelper.getIsSelect(SPHelper.TAG_SELECT_ACTIVATION_CODE))){
            arrayList.add(new ItemSelect(getString(R.string.login_with_activation_code), R.drawable.ic_unlock,false));
        }
        if (Boolean.TRUE.equals(spHelper.getIsSelect(SPHelper.TAG_SELECT_SINGLE))){
            arrayList.add(new ItemSelect(getString(R.string.play_single_stream), R.drawable.ic_movie,false));
        }
        arrayList.add(new ItemSelect(getString(R.string.list_users), R.drawable.ic_user_octagon,true));
        arrayList.add(new ItemSelect(getString(R.string._downloads), R.drawable.iv_downloading,true));

        if (Boolean.TRUE.equals(spHelper.getIsSelect(SPHelper.TAG_IS_LOCAL_STORAGE)) && !isTvBox){
            arrayList.add(new ItemSelect(getString(R.string._local_storage), R.drawable.ic_hard_drive,true));
        }

        addSelectPage(arrayList);

        return arrayList;
    }

    private void addSelectPage(ArrayList<ItemSelect> arrayList) {
        if (arrayList == null){
            return;
        }
        if (arrayListSelectPage != null && !arrayListSelectPage.isEmpty()) {
            for (int i = 0; i < arrayListSelectPage.size(); i++) {
                ItemSelectPage page = arrayListSelectPage.get(i);
                if (page == null) continue; // skip if null just in case

                int iconResId;
                if ("whatsapp".equalsIgnoreCase(page.getType())) {
                    iconResId = R.drawable.ic_whatsapp;
                } else if ("telegram".equalsIgnoreCase(page.getType())) {
                    iconResId = R.drawable.ic_telegram;
                } else {
                    iconResId = R.drawable.ic_link;
                }

                arrayList.add(new ItemSelect(page.getTitle(), iconResId, true));
            }
        }
    }

    private void setupBackPressHandler() {
        OnBackPressedCallback callback = new OnBackPressedCallback(true) {
            @Override
            public void handleOnBackPressed() {
                DialogUtil.exitDialog(SelectPlayerActivity.this);
            }
        };
        getOnBackPressedDispatcher().addCallback(this, callback);
    }

    @OptIn(markerClass = UnstableApi.class)
    private void select(String title) {
        if (title == null){
            return;
        }
        Intent intent;
        if (title.equals(getString(R.string.login_with_xtream_codes))){
            intent = new Intent(SelectPlayerActivity.this, SignInActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "xtream");
            startActivity(intent);
            finish();
        } else if (title.equals(getString(R.string._1_stream))){
            intent = new Intent(SelectPlayerActivity.this, SignInActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "stream");
            startActivity(intent);
            finish();
        } else if (title.equals(getString(R.string.m3u_playlist))){
            intent = new Intent(SelectPlayerActivity.this, AddPlaylistActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "");
            startActivity(intent);
            finish();
        } else if (title.equals(getString(R.string.login_with_device_id))){
            intent = new Intent(SelectPlayerActivity.this, SignInDeviceActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "");
            startActivity(intent);
            finish();
        } else if (title.equals(getString(R.string.play_single_stream))){
            intent = new Intent(SelectPlayerActivity.this, AddSingleURLActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "");
            startActivity(intent);
            finish();
        } else if (title.equals(getString(R.string.list_users))){
            intent = new Intent(SelectPlayerActivity.this, UsersListActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "");
            startActivity(intent);
            finish();
        } else if (title.equals(getString(R.string._downloads))){
            intent = new Intent(SelectPlayerActivity.this, DownloadActivity.class);
            startActivity(intent);
        } else if (title.equals(getString(R.string._local_storage))){
            new SPHelper(this).setLoginType(Callback.TAG_LOGIN_VIDEOS);
            intent = new Intent(SelectPlayerActivity.this, LocalStorageActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "");
            startActivity(intent);
            finish();
        } else if (title.equals(getString(R.string.login_with_activation_code))){
            intent = new Intent(SelectPlayerActivity.this, SignInCodeActivity.class);
            intent.setFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            intent.putExtra("from", "");
            startActivity(intent);
            finish();
        } else {
            openSelectPage(title);
        }
    }

    private void openSelectPage(String title) {
        if (arrayListSelectPage == null || arrayListSelectPage.isEmpty()) {
            return;
        }

        for (int i = 0; i < arrayListSelectPage.size(); i++) {
            ItemSelectPage page = arrayListSelectPage.get(i);
            if (title.equals(page.getTitle())) {
                if ("internal".equals(page.getType())) {
                    openInternalBrowser(page);
                } else {
                    openExternalBrowser(page);
                }
                break; // stop loop after match found
            }
        }
    }

    private void openInternalBrowser(ItemSelectPage item) {
        if (item == null){
            return;
        }
        Intent intent = new Intent(SelectPlayerActivity.this, WebActivity.class);
        intent.putExtra("web_url", item.getBase());
        intent.putExtra("page_title", item.getTitle());
        startActivity(intent);
    }

    private void openExternalBrowser(ItemSelectPage item) {
        if (item == null){
            return;
        }
        try {
            Uri uri = Uri.parse(item.getBase());
            Intent intent = new Intent(Intent.ACTION_VIEW, uri);
            intent.addCategory(Intent.CATEGORY_BROWSABLE);
            intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            if (intent.resolveActivity(getPackageManager()) != null) {
                startActivity(intent);
            } else {
                Toasty.makeText(this, "No browser found to open this link", Toasty.ERROR);
            }
        } catch (Exception e) {
            Toasty.makeText(this, "Invalid URL", Toasty.ERROR);
        }
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_select_player;
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && keyCode == KeyEvent.KEYCODE_BACK){
            DialogUtil.exitDialog(SelectPlayerActivity.this);
            return true;
        } else if (event.getAction() == KeyEvent.ACTION_DOWN && keyCode == KeyEvent.KEYCODE_HOME){
            ThemeHelper.openHomeActivity(this);
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }
}