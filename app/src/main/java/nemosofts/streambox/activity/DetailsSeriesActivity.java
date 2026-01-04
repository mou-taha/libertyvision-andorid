package nemosofts.streambox.activity;

import static android.Manifest.permission.POST_NOTIFICATIONS;
import static android.Manifest.permission.READ_EXTERNAL_STORAGE;
import static android.Manifest.permission.READ_MEDIA_VIDEO;
import static android.Manifest.permission.WRITE_EXTERNAL_STORAGE;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ActivityInfo;
import android.content.pm.PackageManager;
import android.graphics.Bitmap;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.KeyEvent;
import android.view.LayoutInflater;
import android.view.View;
import android.widget.FrameLayout;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.TextView;
import android.widget.Toast;

import androidx.activity.EdgeToEdge;
import androidx.activity.result.ActivityResultLauncher;
import androidx.activity.result.contract.ActivityResultContracts;
import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.core.content.ContextCompat;
import androidx.core.graphics.Insets;
import androidx.core.view.ViewCompat;
import androidx.core.view.WindowInsetsCompat;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.AppCompatActivity;
import androidx.nemosofts.material.BlurImage;
import androidx.nemosofts.material.ProgressDialog;
import androidx.nemosofts.material.Toasty;

import androidx.nemosofts.utils.DeviceUtils;
import androidx.nemosofts.utils.NetworkUtils;
import androidx.nemosofts.utils.YouTubeUtils;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;
import com.squareup.picasso.Target;

import java.util.ArrayList;
import java.util.Collections;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterEpisodes;
import nemosofts.streambox.adapter.AdapterSeason;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.dialog.FeedBackDialog;
import nemosofts.streambox.executor.LoadSeriesID;
import nemosofts.streambox.interfaces.SeriesIDListener;
import nemosofts.streambox.item.ItemEpisodes;
import nemosofts.streambox.item.ItemInfoSeasons;
import nemosofts.streambox.item.ItemSeasons;
import nemosofts.streambox.item.ItemSeries;
import nemosofts.streambox.item.ItemVideoDownload;
import nemosofts.streambox.utils.ApplicationUtil;
import nemosofts.streambox.utils.DownloadNetworkUtils;
import nemosofts.streambox.utils.IfSupported;
import nemosofts.streambox.utils.helper.DBHelper;
import nemosofts.streambox.utils.helper.Helper;
import nemosofts.streambox.utils.helper.JSHelper;
import nemosofts.streambox.utils.helper.SPHelper;
import nemosofts.streambox.utils.helper.ThemeHelper;

public class DetailsSeriesActivity extends AppCompatActivity {

    Helper helper;
    private ActivityResultLauncher<String> permissionLauncher;
    private int playback = 0;
    private DBHelper dbHelper;
    private SPHelper spHelper;
    private String seriesID = "0";
    private String seriesName = "";
    private String seriesRating = "";
    private String seriesCover = "";
    private TextView pageTitle;
    private TextView directed;
    private TextView release;
    private TextView genre;
    private TextView plot;
    private ImageView poster;
    private ImageView star1;
    private ImageView star2;
    private ImageView star3;
    private ImageView star4;
    private ImageView star5;
    private ArrayList<ItemSeasons> arraySeasons;
    private ArrayList<ItemEpisodes> arrayAllEpisodes;
    private ArrayList<ItemEpisodes> arrayEpisodes;
    private RecyclerView rvEpisodes;
    private AdapterEpisodes adapterEpisodes;
    private String seasonID = "0";
    String youtubeURL = "";
    private ImageView btnFav;
    private int themeBg;
    private ProgressDialog progressDialog;
    private LinearLayout llPage;
    private FrameLayout shimmer;
    private boolean isDownloadFeatureEnabled;
    private final Handler downloadStatusHandler = new Handler(Looper.getMainLooper());
    private final Runnable downloadStatusRunnable = this::updateEpisodeDownloadStatuses;
    private boolean isDownloadHandlerRunning = false;

    @Override
    public void onCreate(Bundle savedInstanceState) {
        setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_LANDSCAPE);
        super.onCreate(savedInstanceState);
        EdgeToEdge.enable(this);
        ViewCompat.setOnApplyWindowInsetsListener(findViewById(R.id.theme_bg_details), (v, insets) -> {
            Insets systemInsets = insets.getInsets(WindowInsetsCompat.Type.systemBars());
            v.setPadding(systemInsets.left, systemInsets.top, systemInsets.right, systemInsets.bottom);
            return insets;
        });

        initializeUI();
        initializeViews();
        setupRecyclerView();
        setupActivityResultLaunchers();

        getData();
    }

    private void initializeUI() {
        IfSupported.isRTL(this);
        IfSupported.isScreenshot(this);

        themeBg = ThemeHelper.getThemeBackgroundRes(this);

        ImageView blur = findViewById(R.id.iv_bg_blur);
        blur.setImageResource(themeBg);

        ImageView alpha = findViewById(R.id.iv_alpha);
        alpha.setImageResource(themeBg);

        findViewById(R.id.iv_back_page).setOnClickListener(view -> finish());
        if (DeviceUtils.isTvBox(this)){
            findViewById(R.id.iv_back_page).setVisibility(View.GONE);
        }

        seriesID = getIntent().getStringExtra("series_id");
        seriesName = getIntent().getStringExtra("series_name");
        seriesRating = getIntent().getStringExtra("series_rating");
        seriesCover = getIntent().getStringExtra("series_cover");

        helper = new Helper(this);
        dbHelper = new DBHelper(this);
        spHelper = new SPHelper(this);
        isDownloadFeatureEnabled = spHelper.getIsDownload() && spHelper.getIsDownloadUser();
        helper = new Helper(this, (position, type) -> openPlayerEpisodesActivity(position));
        arraySeasons = new ArrayList<>();
        arrayAllEpisodes = new ArrayList<>();
        arrayEpisodes = new ArrayList<>();
        progressDialog = new ProgressDialog(DetailsSeriesActivity.this);

        LinearLayout adView = findViewById(R.id.ll_adView);
        helper.showBannerAd(adView, Callback.getBannerSeries());
    }

    private void initializeViews() {
        llPage = findViewById(R.id.ll_page);
        shimmer = findViewById(R.id.fl_shimmer);
        pageTitle = findViewById(R.id.tv_page_title);
        poster = findViewById(R.id.iv_series);
        directed = findViewById(R.id.tv_directed);
        release = findViewById(R.id.tv_release);
        genre = findViewById(R.id.tv_genre);
        plot = findViewById(R.id.tv_plot);
        btnFav = findViewById(R.id.iv_fav);

        star1 = findViewById(R.id.iv_star_1);
        star2 = findViewById(R.id.iv_star_2);
        star3 = findViewById(R.id.iv_star_3);
        star4 = findViewById(R.id.iv_star_4);
        star5 = findViewById(R.id.iv_star_5);

        setClickListener();
    }

    private void setupRecyclerView() {
        rvEpisodes = findViewById(R.id.rv_episodes);
        LinearLayoutManager manager = new LinearLayoutManager(this, LinearLayoutManager.VERTICAL, false);
        rvEpisodes.setLayoutManager(manager);
        rvEpisodes.setNestedScrollingEnabled(false);
    }

    private void setupActivityResultLaunchers() {
        permissionLauncher = registerForActivityResult(new ActivityResultContracts.RequestPermission(), isGranted ->
                Toast.makeText(DetailsSeriesActivity.this,
                                Boolean.TRUE.equals(isGranted)
                                        ? "Permission granted"
                                        : getResources().getString(R.string.err_cannot_use_features),
                                Toast.LENGTH_SHORT)
                        .show()
        );
    }

    private void setClickListener() {
        btnFav.setOnClickListener(v -> {
            if (dbHelper.checkSeries(DBHelper.TABLE_FAV_SERIES, seriesID)){
                dbHelper.removeFavSeries(DBHelper.TABLE_FAV_SERIES, seriesID);
                btnFav.setImageResource(R.drawable.ic_favorite_border);
                Toasty.makeText(this, getResources().getString(R.string.fav_remove_success), Toasty.SUCCESS);
            } else {
                ItemSeries itemSeries = new ItemSeries(seriesName,seriesID,seriesCover,seriesRating,"");
                dbHelper.addToSeries(DBHelper.TABLE_FAV_SERIES, itemSeries, 0);
                btnFav.setImageResource(R.drawable.ic_favorite);
                Toasty.makeText(this, getResources().getString(R.string.fav_success), Toasty.SUCCESS);
            }
        });

        findViewById(R.id.ll_play_trailer).setOnClickListener(v -> openYouTubePlayerActivity());
        findViewById(R.id.iv_filter).setOnClickListener(v -> DialogUtil.filterDialog(this, 4, this::setSeasonAdapter));
    }

    private void openYouTubePlayerActivity() {
        if (findViewById(R.id.pb_trailer).getVisibility() == View.GONE && youtubeURL != null && !youtubeURL.isEmpty()) {
            Intent intent = new Intent(DetailsSeriesActivity.this, YouTubePlayerActivity.class);
            intent.putExtra("stream_id", youtubeURL);
            startActivity(intent);
        }
    }

    @OptIn(markerClass = UnstableApi.class)
    private void openPlayerEpisodesActivity(int position) {
        if (arrayEpisodes.isEmpty()){
            return;
        }
        Callback.setPlayPosEpisodes(position);
        if (!Callback.getArrayListEpisodes().isEmpty()) {
            Callback.getArrayListEpisodes().clear();
        }
        Callback.setArrayListEpisodes(arrayEpisodes);
        Intent intent = new Intent(DetailsSeriesActivity.this, ExoPlayerActivity.class);
        intent.putExtra("player_type", ExoPlayerActivity.TAG_TYPE_EPISODES);
        startActivity(intent);
    }

    private void startEpisodeDownload(ItemEpisodes episode) {
        if (episode == null || helper == null || dbHelper == null || spHelper == null) {
            return;
        }
        if (!isDownloadFeatureEnabled) {
            Toasty.makeText(this, getString(R.string.err_cannot_use_features), Toasty.WARNING);
            return;
        }
        if (!NetworkUtils.isConnected(this)){
            Toasty.makeText(this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            return;
        }
        if (!DownloadNetworkUtils.canDownload(this, spHelper)) {
            Toasty.makeText(this, getString(R.string.err_download_network_restricted), Toasty.WARNING);
            return;
        }
        if (!checkPer() || !checkPerNotification()) {
            return;
        }

        if (isEpisodeDownloading(episode.getId())) {
            Toasty.makeText(this, getString(R.string.downloading), Toasty.WARNING);
            return;
        }

        boolean alreadyDownloaded = dbHelper.checkDownload(DBHelper.TABLE_DOWNLOAD_EPISODES,
                episode.getId(),
                ApplicationUtil.containerExtension(episode.getContainerExtension())
        );
        if (alreadyDownloaded) {
            Toasty.makeText(this, getString(R.string.already_download), Toasty.WARNING);
            return;
        }

        ItemVideoDownload download = getItemVideoDownload(episode);
        helper.download(download, DBHelper.TABLE_DOWNLOAD_EPISODES);
    }

    @NonNull
    private ItemVideoDownload getItemVideoDownload(@NonNull ItemEpisodes episode) {
        String coverUrl = episode.getCoverBig();
        if (coverUrl == null || coverUrl.isEmpty()) {
            coverUrl = seriesCover == null ? "" : seriesCover;
        }

        String extension = episode.getContainerExtension();
        if (extension == null || extension.isEmpty()) {
            extension = "mp4";
        }
        ItemVideoDownload download = getItemVideoDownload(episode, extension, coverUrl);
        download.setDownloadTable(DBHelper.TABLE_DOWNLOAD_EPISODES);
        return download;
    }

    @NonNull
    private ItemVideoDownload getItemVideoDownload(@NonNull ItemEpisodes episode, @NonNull String extension, String coverUrl) {
        String sanitizedExtension = extension.startsWith(".") ? extension.substring(1) : extension;
        String fileExtension = ApplicationUtil.containerExtension(extension);

        String episodeUrl = spHelper.getServerURL() + "series/" + spHelper.getUserName() + "/"
                + spHelper.getPassword() + "/" + episode.getId() + "." + sanitizedExtension;

        return new ItemVideoDownload(
                episode.getTitle(),
                episode.getId(),
                coverUrl,
                episodeUrl,
                fileExtension
        );
    }

    private void cancelEpisodeDownload(ItemEpisodes episode) {
        if (episode == null || !isDownloadFeatureEnabled) {
            return;
        }
        if (!isEpisodeDownloading(episode.getId())) {
            return;
        }
        Intent serviceIntent = new Intent(this, DownloadService.class);
        serviceIntent.setAction(DownloadService.ACTION_CANCEL_SINGLE);
        serviceIntent.putExtra("stream_id", episode.getId());
        startService(serviceIntent);
        Toasty.makeText(this, getString(R.string.download_cancelled), Toasty.INFO);
        if (adapterEpisodes != null) {
            adapterEpisodes.updateDownloadQueue(new ArrayList<>(DownloadService.getArrayListVideo()));
        }
    }

    @Override
    public int setContentViewID() {
        return R.layout.activity_details_series;
    }

    private void getData() {
        if (!NetworkUtils.isConnected(this)){
            Toasty.makeText(DetailsSeriesActivity.this, getString(R.string.err_internet_not_connected), Toasty.ERROR);
            return;
        }
        LoadSeriesID loadSeriesID = new LoadSeriesID(this, new SeriesIDListener() {
            @Override
            public void onStart() {
                addShimmer();
            }

            @Override
            public void onEnd(String success,
                              ItemInfoSeasons infoSeasons, ArrayList<ItemSeasons> arrayListSeasons,
                              ArrayList<ItemEpisodes> arrayListEpisodes) {
                if (isFinishing()){
                    return;
                }
                handleLoadSeriesResult(success, infoSeasons, arrayListSeasons, arrayListEpisodes);
            }
        }, ApplicationUtil.getAPIRequestID("get_series_info","series_id", seriesID,
                spHelper.getUserName(), spHelper.getPassword()));
        loadSeriesID.execute();
    }

    private void handleLoadSeriesResult(@NonNull String success, ItemInfoSeasons infoSeasons,
                                        ArrayList<ItemSeasons> arrayListSeasons,
                                        ArrayList<ItemEpisodes> arrayListEpisodes) {
        if (success.equals("1")) {
            if (infoSeasons != null){
                setInfo(infoSeasons);
            }
            if (!arrayListEpisodes.isEmpty()){
                arrayAllEpisodes.addAll(arrayListEpisodes);
            }
            if (!arrayListSeasons.isEmpty()){
                arraySeasons.addAll(arrayListSeasons);
            }

            llPage.setVisibility(View.VISIBLE);
            removeShimmer();
            setSeasonsAdapter();
        }  else {
            if (playback < 2){
                playback = playback + 1;
                Toast.makeText(DetailsSeriesActivity.this, "Checking error - "+ playback +"/2", Toast.LENGTH_SHORT).show();
                getData();
            } else {
                removeShimmer();
                playback = 1;
                Toasty.makeText(DetailsSeriesActivity.this, getString(R.string.err_server_not_connected), Toasty.ERROR);
            }
        }
    }

    private void removeShimmer() {
        if (Boolean.TRUE.equals(spHelper.getIsShimmeringDetails())){
            shimmer.setVisibility(View.GONE);
            shimmer.removeAllViews();
        } else {
            if (progressDialog.isShowing()){
                progressDialog.dismiss();
            }
        }
    }

    private void addShimmer() {
        if (Boolean.TRUE.equals(spHelper.getIsShimmeringDetails())){
            llPage.setVisibility(View.GONE);
            shimmer.setVisibility(View.VISIBLE);
            shimmer.removeAllViews();
            LayoutInflater inflater = (LayoutInflater) getSystemService(Context.LAYOUT_INFLATER_SERVICE);
            @SuppressLint("InflateParams") View myView = inflater.inflate(R.layout.shimmer_series_page, null);
            shimmer.addView(myView);
        } else {
            llPage.setVisibility(View.VISIBLE);
            if (!progressDialog.isShowing()){
                progressDialog.show();
            }
        }
    }

    private void setSeasonsAdapter() {
        RecyclerView rvSeasons = findViewById(R.id.rv_seasons);
        LinearLayoutManager manager = new LinearLayoutManager(this, LinearLayoutManager.HORIZONTAL, false);
        rvSeasons.setLayoutManager(manager);
        rvSeasons.setNestedScrollingEnabled(false);

        if (!arraySeasons.isEmpty()) {
            AdapterSeason adapterColors = new AdapterSeason(this, arraySeasons, (itemSeasons, position) -> {
                seasonID = arraySeasons.get(position).getSeasonNumber();
                setSeasonAdapter();
            });
            rvSeasons.setAdapter(adapterColors);
            seasonID = arraySeasons.get(0).getSeasonNumber();
            setSeasonAdapter();
            if (DeviceUtils.isTvBox(this)){
                rvSeasons.requestFocus();
            }
        } else {
            Toasty.makeText(this, getResources().getString(R.string.err_no_data_found), Toasty.WARNING);
        }
    }

    @SuppressLint("NotifyDataSetChanged")
    private void setSeasonAdapter() {
        if (arrayAllEpisodes.isEmpty()) {
            findViewById(R.id.tv_empty_msg).setVisibility(View.VISIBLE);
            return;
        }

        if (!arrayEpisodes.isEmpty()){
            arrayEpisodes.clear();
        }

        if (!seasonID.equals("0")){
            for (ItemEpisodes episode : arrayAllEpisodes) {
                if (episode.getSeason().equals(seasonID)) {
                    arrayEpisodes.add(episode);
                }
            }
        } else {
            arrayEpisodes.addAll(arrayAllEpisodes);
        }

        if (!arrayEpisodes.isEmpty() && Boolean.TRUE.equals(new JSHelper(this).getIsEpisodesOrder())) {
            Collections.reverse(arrayEpisodes);
        }

        if (!arrayEpisodes.isEmpty()){
            adapterEpisodes = new AdapterEpisodes(this, arrayEpisodes,
                    seriesCover , isDownloadFeatureEnabled,
                    new AdapterEpisodes.RecyclerItemClickListener() {
                        @Override
                        public void onClickListener(ItemEpisodes itemEpisodes, int position) {
                            helper.showInterAd(position,"");
                        }

                        @Override
                        public void onDownloadClick(ItemEpisodes itemEpisodes, int position) {
                            startEpisodeDownload(itemEpisodes);
                        }

                        @Override
                        public void onCancelDownloadClick(ItemEpisodes itemEpisodes, int position) {
                            cancelEpisodeDownload(itemEpisodes);
                        }
                    }
            );
            rvEpisodes.setAdapter(adapterEpisodes);
            if (isDownloadFeatureEnabled) {
                adapterEpisodes.updateDownloadQueue(new ArrayList<>(DownloadService.getArrayListVideo()));
                startDownloadStatusUpdates();
            }
        } else if (adapterEpisodes != null){
            adapterEpisodes.notifyDataSetChanged();
        }
        findViewById(R.id.tv_empty_msg).setVisibility(arrayEpisodes.isEmpty()? View.VISIBLE : View.GONE);
    }

    private void setInfo(ItemInfoSeasons itemInfoSeasons) {
        if (itemInfoSeasons == null){
            return;
        }

        findViewById(R.id.iv_feedback).setOnClickListener(v -> new FeedBackDialog(this).showDialog("Series - "+itemInfoSeasons.getName()));

        pageTitle.setText(itemInfoSeasons.getName());
        directed.setText(itemInfoSeasons.getDirector().isEmpty() || itemInfoSeasons.getDirector().equals("null") ? "N/A" : itemInfoSeasons.getDirector());
        release.setText(itemInfoSeasons.getReleaseDate());
        genre.setText(itemInfoSeasons.getGenre().isEmpty() || itemInfoSeasons.getGenre().equals("null") ? "N/A" : itemInfoSeasons.getGenre());
        plot.setText(itemInfoSeasons.getPlot());

        btnFav.setImageResource(dbHelper.checkSeries(DBHelper.TABLE_FAV_SERIES, seriesID)
                ? R.drawable.ic_favorite
                : R.drawable.ic_favorite_border
        );

        Picasso.get()
                .load(itemInfoSeasons.getCover().isEmpty() ? "null" : itemInfoSeasons.getCover())
                .placeholder(R.drawable.material_design_default)
                .error(R.drawable.material_design_default)
                .into(poster);

        ApplicationUtil.setRating(itemInfoSeasons.getRating5based(), star1, star2, star3, star4, star5);

        setBlur(seriesCover);

        if (itemInfoSeasons.getYoutubeTrailer().isEmpty()){
            findViewById(R.id.ll_play_trailer).setVisibility(View.GONE);
        } else {
            findViewById(R.id.ll_play_trailer).setVisibility(View.VISIBLE);
            if (itemInfoSeasons.getYoutubeTrailer().contains("https://")){
                youtubeURL = YouTubeUtils.getVideoId(itemInfoSeasons.getYoutubeTrailer());
            } else {
                youtubeURL = itemInfoSeasons.getYoutubeTrailer();
            }
        }

        try {
            ItemSeries itemSeries = new ItemSeries(seriesName,seriesID,seriesCover,seriesRating,"");
            dbHelper.addToSeries(DBHelper.TABLE_RECENT_SERIES, itemSeries, spHelper.getMovieLimit());
        } catch (Exception e) {
            ApplicationUtil.log("DetailsSeriesActivity", "setInfo", e);
        }
    }

    private void setBlur(String cover) {
        ImageView blurView = findViewById(R.id.iv_bg_blur);
        if (cover == null){
            blurView.setImageResource(themeBg);
        }
        try {
            Target target = new Target() {
                @Override
                public void onBitmapLoaded(Bitmap bitmap, Picasso.LoadedFrom from) {
                    if (isFinishing() || bitmap == null){
                        return;
                    }
                    try {
                        blurView.setImageBitmap(BlurImage.fastBlur(bitmap, 1f, spHelper.getBlurRadius()));
                    } catch (Exception e) {
                        ApplicationUtil.log("DetailsSeriesActivity", "onBitmapLoaded", e);
                    }
                }

                @Override
                public void onBitmapFailed(Exception e, Drawable errorDrawable) {
                    if (isFinishing()){
                        return;
                    }
                    blurView.setImageResource(themeBg);
                }
                @Override
                public void onPrepareLoad(Drawable placeHolderDrawable) {
                    // this method is empty
                }
            };
            blurView.setTag(target);
            Picasso.get()
                    .load(cover)
                    .placeholder(themeBg)
                    .into(target);

        } catch (Exception e) {
            blurView.setImageResource(themeBg);
        }
    }

    @NonNull
    private Boolean checkPer() {
        String permission;
        if (Build.VERSION.SDK_INT >= 33) {
            permission = READ_MEDIA_VIDEO;
        } else if (Build.VERSION.SDK_INT >= 29) {
            permission = READ_EXTERNAL_STORAGE;
        } else {
            permission = WRITE_EXTERNAL_STORAGE;
        }
        if (ContextCompat.checkSelfPermission(DetailsSeriesActivity.this, permission) != PackageManager.PERMISSION_GRANTED) {
            permissionLauncher.launch(permission);
            return false;
        }
        return true;
    }

    @NonNull
    private Boolean checkPerNotification() {
        if (!DeviceUtils.isTvBox(this) && Build.VERSION.SDK_INT >= 33) {
            if (ContextCompat.checkSelfPermission(DetailsSeriesActivity.this, POST_NOTIFICATIONS) != PackageManager.PERMISSION_GRANTED) {
                permissionLauncher.launch(POST_NOTIFICATIONS);
                return false;
            } else {
                return true;
            }
        }
        return true;
    }

    private void startDownloadStatusUpdates() {
        if (!isDownloadFeatureEnabled || adapterEpisodes == null) {
            return;
        }
        if (!isDownloadHandlerRunning) {
            downloadStatusHandler.post(downloadStatusRunnable);
            isDownloadHandlerRunning = true;
        }
    }

    private void stopDownloadStatusUpdates() {
        downloadStatusHandler.removeCallbacks(downloadStatusRunnable);
        isDownloadHandlerRunning = false;
    }

    private void updateEpisodeDownloadStatuses() {
        if (!isFinishing() && adapterEpisodes != null && isDownloadFeatureEnabled) {
            adapterEpisodes.updateDownloadQueue(new ArrayList<>(DownloadService.getArrayListVideo()));
        }
        downloadStatusHandler.postDelayed(downloadStatusRunnable, 1000);
    }

    private boolean isEpisodeDownloading(String episodeId) {
        if (episodeId == null) {
            return false;
        }
        for (ItemVideoDownload download : DownloadService.getArrayListVideo()) {
            if (download == null) {
                continue;
            }
            if (episodeId.equals(download.getStreamID())
                    && DBHelper.TABLE_DOWNLOAD_EPISODES.equals(download.getDownloadTable())) {
                return true;
            }
        }
        return false;
    }

    @Override
    protected void onResume() {
        super.onResume();
        startDownloadStatusUpdates();
    }

    @Override
    protected void onPause() {
        super.onPause();
        stopDownloadStatusUpdates();
    }

    @Override
    public void onDestroy() {
        if (progressDialog != null && progressDialog.isShowing()){
            progressDialog.cancel();
        }
        stopDownloadStatusUpdates();
        super.onDestroy();
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