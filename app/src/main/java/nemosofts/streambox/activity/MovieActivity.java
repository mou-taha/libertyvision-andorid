package nemosofts.streambox.activity;

import android.annotation.SuppressLint;
import android.app.Activity;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.text.Editable;
import android.text.TextWatcher;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;
import android.widget.FrameLayout;
import android.widget.ProgressBar;
import android.widget.TextView;

import androidx.activity.EdgeToEdge;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.ProgressDialog;

import androidx.nemosofts.utils.DeviceUtils;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import org.jetbrains.annotations.Contract;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterCategory;
import nemosofts.streambox.adapter.AdapterMovie;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.executor.GetCategory;
import nemosofts.streambox.executor.GetMovies;
import nemosofts.streambox.executor.GetMoviesPlaylist;
import nemosofts.streambox.interfaces.GetCategoryListener;
import nemosofts.streambox.interfaces.GetMovieListener;
import nemosofts.streambox.item.ItemCat;
import nemosofts.streambox.item.ItemMovies;
import nemosofts.streambox.utils.ApplicationUtil;
import nemosofts.streambox.utils.IfSupported;
import nemosofts.streambox.utils.helper.Helper;
import nemosofts.streambox.utils.helper.ThemeHelper;
import nemosofts.streambox.utils.recycler.EndlessRecyclerViewScrollListener;
import nemosofts.streambox.utils.recycler.OptimizedItemAnimator;

public class MovieActivity extends AppCompatActivity {

    public static final String TAG_TYPE_PLAYLIST = "playlist";
    public static final String TAG_TYPE_ONLINE = "online";
    private String pageType = TAG_TYPE_ONLINE;

    Helper helper;
    private ProgressDialog progressDialog;
    // Category
    private AdapterCategory adapterCategory;
    private RecyclerView rvCat;
    private ArrayList<ItemCat> arrayListCat;
    // Movies
    private FrameLayout frameLayout;
    private Boolean isOver = false;
    private Boolean isScroll = false;
    private Boolean isLoading = false;
    private int page = 1;
    private AdapterMovie adapter;
    private ArrayList<ItemMovies> arrayList;
    private RecyclerView rv;
    private ProgressBar pb;
    private int isPage = 0;
    private GetMovies loadMovies;
    private GetMoviesPlaylist loadMoviesPlaylist;
    private int pos = 1;

    private String selectCatID = "0";
    private static final String TAG_CAT_ID = "cat_id";

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
        pageType = intent.getStringExtra("pageType");
        if (intent.hasExtra(TAG_CAT_ID)){
            selectCatID = intent.getStringExtra(TAG_CAT_ID);
        }

        initializeUI();
        setupRecyclerView();
        setupListeners();

        new Handler(Looper.getMainLooper()).postDelayed(this::getDataCat, 0);
    }

    private void initializeUI() {
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);

        TextView title = findViewById(R.id.tv_page_title);
        title.setText(getString(R.string.movies_home));

        findViewById(R.id.theme_bg).setBackgroundResource(ThemeHelper.getThemeBackgroundRes(this));
        findViewById(R.id.iv_back_page).setOnClickListener(view -> finish());
        if (DeviceUtils.isTvBox(this)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        pb = findViewById(R.id.pb);
        frameLayout = findViewById(R.id.fl_empty);
        rv = findViewById(R.id.rv);
        rvCat = findViewById(R.id.rv_cat);

        progressDialog = new ProgressDialog(MovieActivity.this);
        helper = new Helper(this, (position, type) -> openDetailsMovieActivity(position));
        arrayList = new ArrayList<>();
        arrayListCat = new ArrayList<>();
    }

    @OptIn(markerClass = UnstableApi.class)
    private void openDetailsMovieActivity(int position) {
        Intent intent;
        if (pageType.equals(TAG_TYPE_PLAYLIST)){
            intent = new Intent(MovieActivity.this, ExoPlayerActivity.class);
            intent.putExtra("player_type", ExoPlayerActivity.TAG_TYPE_SINGLE_URL);
            intent.putExtra("video_name", arrayList.get(position).getName());
            intent.putExtra("video_url", arrayList.get(position).getStreamID());
        } else {
            intent = new Intent(MovieActivity.this, DetailsMovieActivity.class);
            intent.putExtra("stream_id", arrayList.get(position).getStreamID());
            intent.putExtra("stream_name", arrayList.get(position).getName());
            intent.putExtra("stream_icon", arrayList.get(position).getStreamIcon());
            intent.putExtra("stream_rating", arrayList.get(position).getRating());
        }
        startActivity(intent);
    }

    private void setupRecyclerView() {
        GridLayoutManager grid = new GridLayoutManager(this, 1);
        grid.setSpanCount(DeviceUtils.isTvBox(this) ? 6 : 5);
        rv.setLayoutManager(grid);
        rv.setItemAnimator(OptimizedItemAnimator.create());
        rv.addOnScrollListener(new EndlessRecyclerViewScrollListener(grid) {
            @Override
            public void onLoadMore(int p, int totalItemsCount) {
                if (isFinishing()){
                    return;
                }
                if (isPage == 0 && (Boolean.FALSE.equals(isOver) && (Boolean.FALSE.equals(isLoading)))) {
                    isLoading = true;
                    new Handler(Looper.getMainLooper()).postDelayed(() -> {
                        isScroll = true;
                        getData();
                    }, 0);
                }
            }
        });

        LinearLayoutManager llm = new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false);
        rvCat.setLayoutManager(llm);
        rvCat.setItemAnimator(OptimizedItemAnimator.create());

    }

    private void setupListeners() {
        findViewById(R.id.btn_filter).setOnClickListener(v -> DialogUtil.filterDialog(this, 2, () -> recreateData(pos)));
        findViewById(R.id.iv_search).setOnClickListener(view -> {
            Intent intent = new Intent(MovieActivity.this, SearchActivity.class);
            if (pageType.equals(TAG_TYPE_PLAYLIST)){
                intent.putExtra("page", "MoviePlaylist");
            } else {
                intent.putExtra("page", "Movie");
            }
            startActivity(intent);
        });

        if (pageType.equals(TAG_TYPE_PLAYLIST)){
            findViewById(R.id.btn_cat_filter).setVisibility(View.GONE);
        } else {
            findViewById(R.id.btn_cat_filter).setOnClickListener(v -> {
                Intent result = new Intent(MovieActivity.this, FilterCategoriesActivity.class);
                result.putExtra("cat_type", "movie");
                filterLauncher.launch(result);
            });
        }
    }

    private final ActivityResultLauncher<Intent> filterLauncher = registerForActivityResult(
            new ActivityResultContracts.StartActivityForResult(), result -> {
                if (result.getResultCode() == Activity.RESULT_OK && result.getData() != null) {
                    recreate();
                }
            }
    );

    private void getDataCat() {
        new GetCategory(this, pageType.equals(TAG_TYPE_PLAYLIST)
                ? GetCategory.PAGE_TYPE_PLAYLIST_5
                : GetCategory.PAGE_TYPE_MOVIE, new GetCategoryListener() {
            @Override
            public void onStart() {
                progressDialog.show();
            }

            @Override
            public void onEnd(String success, ArrayList<ItemCat> itemCat) {
                if (isFinishing()){
                    return;
                }
                progressDialog.dismiss();
                if (success.equals("1") && !itemCat.isEmpty()) {
                    endCategory(itemCat);
                    return;
                }

                setEmpty();
            }
        }).execute();
    }

    private void endCategory(ArrayList<ItemCat> itemCat) {
        if (!arrayListCat.isEmpty()){
            arrayListCat.clear();
        }
        if (pageType.equals(TAG_TYPE_PLAYLIST)){
            selectCatID = itemCat.get(0).getName();
        } else {
            if (selectCatID.equals("0")){
                selectCatID = itemCat.get(0).getId();
            }
            arrayListCat.add(new ItemCat("01",getString(R.string.favourite),""));
            arrayListCat.add(new ItemCat("02",getString(R.string.recently),""));
            arrayListCat.add(new ItemCat("03",getString(R.string.recently_add),""));
        }
        arrayListCat.addAll(itemCat);
        setAdapterToCatListview();
    }

    private void getData() {
        if (pageType.equals(TAG_TYPE_PLAYLIST)){
            getDataPlaylist();
            return;
        }
        loadMovies = new GetMovies(this, page, selectCatID, isPage, new GetMovieListener() {
            @Override
            public void onStart() {
                if (arrayList.isEmpty()){
                    pb.setVisibility(View.VISIBLE);
                    frameLayout.setVisibility(View.GONE);
                }
            }

            @Override
            public void onEnd(String success, ArrayList<ItemMovies> arrayListMovies) {
                if (isFinishing()){
                    return;
                }
                pb.setVisibility(View.GONE);
                if (Boolean.TRUE.equals(isOver)){
                    return;
                }
                if (success.equals("1")) {
                    if (arrayListMovies.isEmpty()) {
                        isOver = true;
                        setEmpty();
                    } else {
                        page = page + 1;
                        int newItems = arrayListMovies.size();
                        arrayList.addAll(arrayListMovies);
                        setAdapterToListview(newItems);
                    }
                } else {
                    setEmpty();
                }
                isLoading = false;
            }
        });
        loadMovies.execute();
    }

    private void getDataPlaylist() {
        loadMoviesPlaylist = new GetMoviesPlaylist(this, page, selectCatID, new GetMovieListener() {
            @Override
            public void onStart() {
                if (arrayList.isEmpty()){
                    pb.setVisibility(View.VISIBLE);
                    frameLayout.setVisibility(View.GONE);
                }
            }

            @Override
            public void onEnd(String success, ArrayList<ItemMovies> arrayListMovies) {
                if (isFinishing()){
                    return;
                }
                pb.setVisibility(View.GONE);
                if (Boolean.TRUE.equals(isOver)){
                    return;
                }
                if (success.equals("1")) {
                    if (arrayListMovies.isEmpty()) {
                        isOver = true;
                        setEmpty();
                    } else {
                        page = page + 1;
                        int newItems = arrayListMovies.size();
                        arrayList.addAll(arrayListMovies);
                        setAdapterToListview(newItems);
                    }
                } else {
                    setEmpty();
                }
                isLoading = false;
            }
        });
        loadMoviesPlaylist.execute();
    }


    public void setAdapterToCatListview() {
        adapterCategory = new AdapterCategory(this, arrayListCat, position -> {
            if (pos != position){
                recreateData(position);
            }
        });
        rvCat.setAdapter(adapterCategory);
        if (selectCatID.equals("0")){
            adapterCategory.select(pageType.equals(TAG_TYPE_PLAYLIST) ? 0 : 3);
        } else {
            adapterCategory.selectID(selectCatID);
        }
        pos = pageType.equals(TAG_TYPE_PLAYLIST) ? 0 : 3;

        handleAdultContentVerification();
        setupSearchFunctionality();
    }

    private void setupSearchFunctionality() {
        EditText edtSearch = findViewById(R.id.edt_search);
        edtSearch.setOnEditorActionListener((v, actionId, event) -> {
            hideKeyboard();
            return true;
        });
        edtSearch.addTextChangedListener(createSearchWatcher());
    }

    private void hideKeyboard() {
        InputMethodManager inputManager = (InputMethodManager) getSystemService(Context.INPUT_METHOD_SERVICE);
        View currentFocus = getCurrentFocus();
        if (currentFocus != null) {
            inputManager.hideSoftInputFromWindow(currentFocus.getWindowToken(), InputMethodManager.HIDE_NOT_ALWAYS);
        }
    }

    @NonNull
    @Contract(" -> new")
    private TextWatcher createSearchWatcher() {
        return new TextWatcher() {
            @Override
            public void beforeTextChanged(CharSequence s, int start, int count, int after) {
                // this method is empty
            }

            @Override
            public void onTextChanged(CharSequence s, int start, int before, int count) {
                if (adapterCategory != null) {
                    adapterCategory.getFilter().filter(s);
                }
            }

            @Override
            public void afterTextChanged(Editable s) {
                // this method is empty
            }
        };
    }

    @SuppressLint("NotifyDataSetChanged")
    private void recreateData(int position) {
        if (position >= 0 && position < arrayListCat.size()) {
            pos = position;

            if (pageType.equals(TAG_TYPE_PLAYLIST)){
                selectCatID = arrayListCat.get(position).getName();
            } else {
                selectCatID = arrayListCat.get(position).getId();
            }
            adapterCategory.select(position);

            cancelTask();

            if (!pageType.equals(TAG_TYPE_PLAYLIST)){
                determinePageType(position);
            }

            new Handler(Looper.getMainLooper()).postDelayed(this::resetPaginationAndFetchData, 0);
        }
    }

    private void cancelTask() {
        // Cancel any ongoing task
        if (loadMoviesPlaylist != null) {
            loadMoviesPlaylist.shutDown();
        }
        if (loadMovies != null) {
            loadMovies.shutDown();
        }
        isOver = true;

        // Clear the list
        if (!arrayList.isEmpty()){
            arrayList.clear();
        }

        // Notify adapter of data change
        if (adapter != null){
            adapter.notifyDataSetChanged();
        }
    }

    private void determinePageType(int position) {
        switch (arrayListCat.get(position).getId()) {
            case "01":
                isPage = 1; // Favorites
                break;
            case "02":
                isPage = 2; // Recently watched
                break;
            case "03":
                isPage = 3; // Recently added
                break;
            default:
                isPage = 0; // Default category
                break;
        }
    }

    private void resetPaginationAndFetchData() {
        isOver = false;
        isScroll = false;
        isLoading = false;
        page = 1;

        handleAdultContentVerification();
    }

    private void handleAdultContentVerification() {
        if (ApplicationUtil.isAdultsCount(arrayListCat.get(pos).getName())) {
            DialogUtil.childCountDialog(this, pos, position -> getData());
        } else {
            new Handler(Looper.getMainLooper()).postDelayed(this::getData, 0);
        }
    }

    public void setAdapterToListview(int newItemsCount) {
        if(Boolean.FALSE.equals(isScroll)) {
            adapter = new AdapterMovie(this, arrayList, (itemCat, position) -> helper.showInterAd(position,""));
            rv.setAdapter(adapter);
            rv.requestFocus();
            setEmpty();
        } else if (adapter != null && newItemsCount > 0) {
            int start = Math.max(0, arrayList.size() - newItemsCount);
            adapter.notifyItemRangeInserted(start, newItemsCount);
        }
    }

    private void setEmpty() {
        if (!arrayList.isEmpty()) {
            rv.setVisibility(View.VISIBLE);
            frameLayout.setVisibility(View.GONE);
        } else {
            rv.setVisibility(View.GONE);
            frameLayout.setVisibility(View.VISIBLE);

            frameLayout.removeAllViews();

            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);

            @SuppressLint("InflateParams") View myView = inflater.inflate(R.layout.row_empty, null);

            frameLayout.addView(myView);
        }
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_list;
    }

    @Override
    public boolean onKeyDown(int keyCode, @NonNull KeyEvent event) {
        if (event.getAction() == KeyEvent.ACTION_DOWN && keyCode == KeyEvent.KEYCODE_BACK){
            finish();
            return true;
        } else if (event.getAction() == KeyEvent.ACTION_DOWN &&  keyCode == KeyEvent.KEYCODE_HOME){
            ThemeHelper.openHomeActivity(this);
            return true;
        }
        return super.onKeyDown(keyCode, event);
    }

    @Override
    public void onDestroy() {
        if (progressDialog != null && progressDialog.isShowing()){
            progressDialog.cancel();
        }
        Picasso.get().cancelTag(this);
        super.onDestroy();
    }
}