package nemosofts.streambox.fragment;

import android.annotation.SuppressLint;
import android.content.Context;
import android.content.Intent;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;
import android.widget.FrameLayout;
import android.widget.ProgressBar;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.fragment.app.Fragment;
import androidx.media3.common.util.UnstableApi;
import androidx.nemosofts.material.IconTextView;
import androidx.nemosofts.material.ProgressDialog;
import androidx.nemosofts.material.Toasty;

import androidx.nemosofts.utils.DeviceUtils;
import androidx.nemosofts.utils.NetworkUtils;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import org.jetbrains.annotations.Contract;

import java.util.ArrayList;

import nemosofts.streambox.R;
import nemosofts.streambox.activity.ExoPlayerLiveActivity;
import nemosofts.streambox.adapter.AdapterCategory;
import nemosofts.streambox.adapter.AdapterChannel;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.dialog.DialogUtil;
import nemosofts.streambox.executor.GetCategory;
import nemosofts.streambox.executor.GetChannel;
import nemosofts.streambox.executor.LoadChannel;
import nemosofts.streambox.interfaces.GetCategoryListener;
import nemosofts.streambox.interfaces.GetChannelListener;
import nemosofts.streambox.interfaces.LoadSuccessListener;
import nemosofts.streambox.item.ItemCat;
import nemosofts.streambox.item.ItemChannel;
import nemosofts.streambox.utils.ApplicationUtil;
import nemosofts.streambox.utils.helper.Helper;
import nemosofts.streambox.utils.helper.SPHelper;
import nemosofts.streambox.utils.recycler.EndlessRecyclerViewScrollListener;

public class FragmentLive extends Fragment {

    @NonNull
    @Contract(" -> new")
    public static FragmentLive newInstance() {
        return new FragmentLive();
    }

    Helper helper;
    private View rootView;
    private SPHelper spHelper;
    private ProgressDialog progressDialog;
    // Category
    private AdapterCategory adapterCategory;
    private RecyclerView rvCat;
    private ArrayList<ItemCat> arrayListCat;
    // Live
    private FrameLayout frameLayout;
    private Boolean isOver = false;
    private Boolean isScroll = false;
    private Boolean isLoading = false;
    private int page = 1;
    private AdapterChannel adapter;
    private ArrayList<ItemChannel> arrayList;
    private RecyclerView rv;
    private ProgressBar pb;
    private int isPage = 0;
    private GetChannel loadLive;
    private int pos = 1;
    boolean load = false;

    private IconTextView btnDownload;
    private final Handler handler = new Handler(Looper.getMainLooper());
    private ProgressBar pbDownload;
    private int progressStatus = 0;

    private String selectCatID = "0";

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        rootView = inflater.inflate(R.layout.fragment_live, container, false);

        progressDialog = new ProgressDialog(requireContext());
        helper = new Helper(requireActivity(), (position, type) -> setPlayerLiveActivity(position));
        spHelper = new SPHelper(requireContext());
        arrayList = new ArrayList<>();
        arrayListCat = new ArrayList<>();

        pb = rootView.findViewById(R.id.pb);
        frameLayout = rootView.findViewById(R.id.fl_empty);
        rv = rootView.findViewById(R.id.rv);
        rvCat = rootView.findViewById(R.id.rv_cat);

        btnDownload = rootView.findViewById(R.id.btn_download);
        pbDownload = rootView.findViewById(R.id.pb_download);
        btnDownload.setOnClickListener(v -> {
            btnDownload.setVisibility(View.GONE);
            getLive();
        });

        setupRecyclerView();
        return rootView;
    }

    private void setupRecyclerView() {
        Context context = requireContext();
        GridLayoutManager grid = new GridLayoutManager(context, 1);
        grid.setSpanCount(DeviceUtils.isTvBox(context) ? 6 : 5);
        rv.setLayoutManager(grid);
        rv.setItemAnimator(new DefaultItemAnimator());
        rv.addOnScrollListener(new EndlessRecyclerViewScrollListener(grid) {
            @Override
            public void onLoadMore(int p, int totalItemsCount) {
                if (getActivity() == null){
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

        LinearLayoutManager manager = new LinearLayoutManager(context, LinearLayoutManager.VERTICAL, false);
        rvCat.setLayoutManager(manager);
        rvCat.setItemAnimator(new DefaultItemAnimator());
    }

    public void loadInitialData() {
        if (!load){
            if (spHelper.getCurrent(Callback.TAG_TV).isEmpty()){
                btnDownload.setVisibility(View.VISIBLE);
                if (DeviceUtils.isTvBox(requireContext())){
                    btnDownload.requestFocus();
                }
            } else {
                getLive();
            }
        }
    }

    private void getLive() {
        if (getActivity() == null){
            return;
        }

        if (!spHelper.getCurrent(Callback.TAG_TV).isEmpty()){
            getDataCat();
            return;
        }

        if (!NetworkUtils.isConnected(requireContext())){
            progressDialog.dismiss();
            Toasty.makeText(requireActivity(), getString(R.string.err_internet_not_connected), Toasty.ERROR);
            return;
        }

        new LoadChannel(requireActivity(), new LoadSuccessListener() {
            @Override
            public void onStart() {
                progressDialog.show();
                showProgress(true);
            }

            @Override
            public void onEnd(String success, String msg) {
                if (getActivity() == null){
                    return;
                }
                progressDialog.dismiss();
                if (success.equals("1")) {
                    showProgress(false);
                    spHelper.setCurrentDate(Callback.TAG_TV);
                    getDataCat();
                    Toast.makeText(requireActivity(), getString(R.string.added_success), Toast.LENGTH_SHORT).show();
                }  else {
                    Toasty.makeText(requireActivity(), getString(R.string.err_server_not_connected), Toasty.ERROR);
                }
            }
        }).execute();
    }

    private void showProgress(boolean status) {
        if (status){
            pbDownload.setVisibility(View.VISIBLE);
            progressStatus = 0;
            pbDownload.setProgress(progressStatus);
            handler.postDelayed(new Runnable() {
                @Override
                public void run() {
                    if (progressStatus < 50) {
                        progressStatus++;
                        pbDownload.setProgress(progressStatus);
                        handler.postDelayed(this, 20);
                    }
                }
            }, 20);
            return;
        }

        handler.postDelayed(new Runnable() {
            @Override
            public void run() {
                if (progressStatus < 100) {
                    progressStatus++;
                    pbDownload.setProgress(progressStatus);
                    if (progressStatus == 99){
                        pbDownload.setVisibility(View.GONE);
                    }
                    handler.postDelayed(this, 10);
                }
            }
        }, 10);
    }

    @OptIn(markerClass = UnstableApi.class)
    private void setPlayerLiveActivity(int position) {
        Callback.setPlayPosLive(position);
        if (!Callback.getArrayListLive().isEmpty()) {
            Callback.getArrayListLive().clear();
        }
        Callback.setArrayListLive(arrayList);
        startActivity(new Intent(requireActivity(), ExoPlayerLiveActivity.class));
    }

    private void getDataCat() {
        rootView.findViewById(R.id.ll_live_view).setVisibility(View.VISIBLE);
        new GetCategory(requireContext(), GetCategory.PAGE_TYPE_LIVE, new GetCategoryListener() {
            @Override
            public void onStart() {
                progressDialog.show();
            }

            @Override
            public void onEnd(String success, ArrayList<ItemCat> itemCat) {
                if (getActivity() == null){
                    return;
                }
                progressDialog.dismiss();
                if (success.equals("1") && !itemCat.isEmpty()) {
                    if (!arrayListCat.isEmpty()){
                        arrayListCat.clear();
                    }
                    selectCatID = itemCat.get(0).getId();
                    arrayListCat.add(new ItemCat("01",getString(R.string.favourite),""));
                    arrayListCat.add(new ItemCat("02",getString(R.string.recently),""));
                    arrayListCat.add(new ItemCat("03",getString(R.string.recently_add),""));
                    arrayListCat.addAll(itemCat);
                    setAdapterToCatListview();
                    load = true;
                } else {
                    load = false;
                    setEmpty();
                }
            }
        }).execute();
    }

    private void getData() {
        loadLive = new GetChannel(requireContext(), page, selectCatID, isPage, new GetChannelListener() {
            @Override
            public void onStart() {
                if (arrayList.isEmpty()){
                    pb.setVisibility(View.VISIBLE);
                    frameLayout.setVisibility(View.GONE);
                }
            }

            @Override
            public void onEnd(String success, ArrayList<ItemChannel> arrayListLive) {
                if (getActivity() == null){
                    return;
                }
                pb.setVisibility(View.GONE);
                if (Boolean.TRUE.equals(isOver)){
                    return;
                }

                if (success.equals("1")) {
                    if (arrayListLive.isEmpty()) {
                        isOver = true;
                        setEmpty();
                    } else {
                        arrayList.addAll(arrayListLive);
                        page = page + 1;
                        setAdapterToListview();
                    }
                } else {
                    setEmpty();
                }
                isLoading = false;
            }
        });
        loadLive.execute();
    }

    public void setAdapterToCatListview() {
        adapterCategory = new AdapterCategory(requireContext(), arrayListCat, position -> {
            if (pos != position){
                recreateData(position);
            }
        });
        rvCat.setAdapter(adapterCategory);
        adapterCategory.select(3);
        pos = 3;

        handleAdultContentVerification();
        setupSearchFunctionality();
    }

    private void setupSearchFunctionality() {
        EditText edtSearch = rootView.findViewById(R.id.edt_search);
        edtSearch.setOnEditorActionListener((v, actionId, event) -> {
            hideKeyboard();
            return true;
        });

        edtSearch.addTextChangedListener(ApplicationUtil.createSearchWatcher(adapterCategory));
    }

    private void hideKeyboard() {
        InputMethodManager inputManager = (InputMethodManager) requireActivity().getSystemService(Context.INPUT_METHOD_SERVICE);
        View currentFocus = requireActivity().getCurrentFocus();
        if (currentFocus != null) {
            inputManager.hideSoftInputFromWindow(currentFocus.getWindowToken(), InputMethodManager.HIDE_NOT_ALWAYS);
        }
    }

    @SuppressLint("NotifyDataSetChanged")
    private void recreateData(int position) {
        if (position >= 0 && position < arrayListCat.size()) {
            pos = position;

            selectCatID = arrayListCat.get(position).getId();
            adapterCategory.select(position);

            if (loadLive != null) {
                loadLive.shutDown();
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

            isPage = ApplicationUtil.determinePageType(arrayListCat, position);

            new Handler(Looper.getMainLooper()).postDelayed(this::resetPaginationAndFetchData, 0);
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
            DialogUtil.childCountDialog(requireContext(), pos, position -> getData());
        } else {
            new Handler(Looper.getMainLooper()).postDelayed(this::getData, 0);
        }
    }

    public void setAdapterToListview() {
        if(Boolean.FALSE.equals(isScroll)) {
            adapter = new AdapterChannel(requireContext(), arrayList, (itemCat, position) -> helper.showInterAd(position,""));
            rv.setAdapter(adapter);
            rv.requestFocus();
            setEmpty();
        } else {
            adapter.notifyItemInserted(arrayList.size()-1);
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

            LayoutInflater inflater = (LayoutInflater) requireActivity().getSystemService(Context.LAYOUT_INFLATER_SERVICE);

            @SuppressLint("InflateParams") View myView = inflater.inflate(R.layout.row_empty, null);

            frameLayout.addView(myView);
        }
    }

    @Override
    public void onDestroy() {
        if (progressDialog != null && progressDialog.isShowing()){
            progressDialog.cancel();
        }
        super.onDestroy();
    }
}