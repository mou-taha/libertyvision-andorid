package nemosofts.streambox.fragment;

import android.annotation.SuppressLint;
import android.content.Context;
import android.os.Bundle;
import android.os.Handler;
import android.os.Looper;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ProgressBar;
import android.widget.Toast;

import androidx.annotation.NonNull;
import androidx.fragment.app.Fragment;
import androidx.nemosofts.material.IconTextView;
import androidx.nemosofts.material.ProgressDialog;
import androidx.nemosofts.material.Toasty;

import androidx.nemosofts.utils.DeviceUtils;
import androidx.nemosofts.utils.NetworkUtils;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.LinearLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import org.jetbrains.annotations.Contract;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.adapter.AdapterHome;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.executor.LoadSeries;
import nemosofts.streambox.interfaces.LoadSuccessListener;
import nemosofts.streambox.item.ItemCat;
import nemosofts.streambox.item.ItemPostHome;
import nemosofts.streambox.item.ItemSeries;
import nemosofts.streambox.utils.ApplicationUtil;
import nemosofts.streambox.utils.AsyncTaskExecutor;
import nemosofts.streambox.utils.helper.JSHelper;
import nemosofts.streambox.utils.helper.SPHelper;

public class FragmentSeries extends Fragment {

    @NonNull
    @Contract(" -> new")
    public static FragmentSeries newInstance() {
        return new FragmentSeries();
    }

    private static final String TAG = "FragmentMovie";
    private static final int ITEMS_PER_PAGE = 10;
    private static final int INITIAL_ITEMS_PER_PAGE = 5;
    private static final int SLIDER_ITEMS_LIMIT = 2;
    private static final int CATEGORY_ITEMS_LIMIT = 7;

    private JSHelper jsHelper;
    private SPHelper sharedPref;
    private RecyclerView recyclerView;
    private final ArrayList<ItemPostHome> arrayListPost = new ArrayList<>();
    private final ArrayList<ItemPostHome> arrayListPostAll = new ArrayList<>();
    private AdapterHome adapterHome;
    private ProgressDialog progressDialog;
    private ProgressBar progressBar;
    private IconTextView loadMoreButton;
    private ProgressBar loadMoreProgressBar;
    private Boolean isLoadingMore = false;
    private int currentPage = 1;
    boolean load = false;

    private IconTextView btnDownload;
    private final Handler handler = new Handler(Looper.getMainLooper());
    private ProgressBar pbDownload;
    private int progressStatus = 0;

    @Override
    public View onCreateView(@NonNull LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View rootView = inflater.inflate(R.layout.fragment_movie, container, false);
        initializeViews(rootView);
        setupRecyclerView();
        return rootView;
    }

    private void initializeViews(View rootView) {
        Context context = requireContext();
        jsHelper = new JSHelper(context);
        sharedPref = new SPHelper(context);
        progressDialog = new ProgressDialog(context);

        loadMoreButton = rootView.findViewById(R.id.btn_load_more);
        loadMoreProgressBar = rootView.findViewById(R.id.pb_load_more);
        progressBar = rootView.findViewById(R.id.pb);
        recyclerView = rootView.findViewById(R.id.rv);

        btnDownload = rootView.findViewById(R.id.btn_download);
        pbDownload = rootView.findViewById(R.id.pb_download);

        loadMoreButton.setOnClickListener(v -> loadMoreData());

        btnDownload.setOnClickListener(v -> {
            btnDownload.setVisibility(View.GONE);
            getSeries();
        });
    }

    private void setupRecyclerView() {
        LinearLayoutManager layoutManager = new LinearLayoutManager(getActivity());
        recyclerView.setLayoutManager(layoutManager);
        recyclerView.setItemAnimator(new DefaultItemAnimator());
    }

    public void loadInitialData() {
        if (!load){
            if (sharedPref.getCurrent(Callback.TAG_SERIES).isEmpty()){
                btnDownload.setVisibility(View.VISIBLE);
                if (DeviceUtils.isTvBox(requireContext())){
                    btnDownload.requestFocus();
                }
            } else {
                getSeries();
            }
        }
    }

    @SuppressLint("StaticFieldLeak")
    private void loadMoreData() {
        new AsyncTaskExecutor<String, String, String>() {

            final ArrayList<ItemPostHome> arrayList = new ArrayList<>();

            @Override
            protected void onPreExecute() {
                isLoadingMore = true;
                currentPage = currentPage + 1;
                loadMoreButton.setVisibility(View.GONE);
                loadMoreProgressBar.setVisibility(View.VISIBLE);
                super.onPreExecute();
            }


            @Override
            protected String doInBackground(String strings) {
                try {
                    if (!arrayListPostAll.isEmpty()){
                        int startIndex = (currentPage - 1) * ITEMS_PER_PAGE;
                        int endIndex = Math.min(startIndex + ITEMS_PER_PAGE, arrayListPostAll.size());
                        for (int o = startIndex; o < endIndex; o++) {
                            arrayList.add(arrayListPostAll.get(o));
                        }
                    }
                    return "1";
                } catch (Exception e) {
                    return "0";
                }
            }

            @Override
            protected void onPostExecute(String s) {
                if (getActivity() == null){
                    return;
                }
                setLoadMoreData(s, arrayList);
            }
        }.execute();
    }

    private void setLoadMoreData(String s, ArrayList<ItemPostHome> arrayList) {
        if (s.equals("1")){
            if (arrayList.isEmpty()){
                loadMoreProgressBar.setVisibility(View.GONE);
            } else {
                arrayListPost.addAll(arrayList);
                setAdapter();
                loadMoreButton.setVisibility(View.VISIBLE);
                loadMoreProgressBar.setVisibility(View.GONE);
            }
        } else {
            loadMoreProgressBar.setVisibility(View.GONE);
        }
    }

    private void getSeries() {
        if (getActivity() == null){
            return;
        }

        if (!sharedPref.getCurrent(Callback.TAG_SERIES).isEmpty()){
            new LoadDataTask().execute();
            return;
        }

        if (!NetworkUtils.isConnected(requireContext())){
            progressDialog.dismiss();
            Toasty.makeText(requireActivity(), getString(R.string.err_internet_not_connected), Toasty.ERROR);
            return;
        }

        new LoadSeries(requireActivity(),  new LoadSuccessListener() {
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
                    sharedPref.setCurrentDate(Callback.TAG_SERIES);
                    new LoadDataTask().execute();
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

    private class LoadDataTask extends AsyncTaskExecutor<String, String, String> {

        ArrayList<ItemCat> arrayListCat = new ArrayList<>();
        ArrayList<ItemSeries> arrayListSeries = new ArrayList<>();

        @Override
        protected void onPreExecute() {
            progressBar.setVisibility(View.VISIBLE);
            super.onPreExecute();
        }

        @Override
        protected String doInBackground(String strings) {
            try {
                arrayListCat.addAll(jsHelper.fetchAllCategories(JSHelper.TAG_JSON_SERIES_CAT));
                arrayListSeries.addAll(jsHelper.getSeriesRe());

                if (!arrayListSeries.isEmpty()){
                    Collections.sort(arrayListSeries, (o1, o2) ->
                            Integer.compare(
                                    Integer.parseInt(o1.getSeriesID()), Integer.parseInt(o2.getSeriesID())
                            )
                    );
                    Collections.reverse(arrayListSeries);
                }

                if (!arrayListCat.isEmpty() && !arrayListSeries.isEmpty()){

                    addSliderList(arrayListPostAll, arrayListSeries);

                    addSeriesList(arrayListCat, arrayListPostAll, arrayListSeries);

                    if (!arrayListPostAll.isEmpty()){
                        int startIndex = (currentPage - 1) * INITIAL_ITEMS_PER_PAGE;
                        int endIndex = Math.min(startIndex + INITIAL_ITEMS_PER_PAGE, arrayListPostAll.size());
                        for (int o = startIndex; o < endIndex; o++) {
                            arrayListPost.add(arrayListPostAll.get(o));
                        }
                    }
                    return "1";
                } else {
                    return "0";
                }
            } catch (Exception e) {
                return "0";
            }
        }

        @Override
        protected void onPostExecute(String success) {
            if (getActivity() == null) {
                return;
            }
            if (success.equals("1")) {
                setAdapter();
                load = true;
            } else {
                load = false;
                progressBar.setVisibility(View.GONE);
                sharedPref.setCurrentDateEmpty(Callback.TAG_SERIES);
                Toasty.makeText(getActivity(), getString(R.string.err_no_data_found), Toasty.ERROR);
            }
        }
    }

    @SuppressLint("NotifyDataSetChanged")
    private void setAdapter() {
        if(Boolean.FALSE.equals(isLoadingMore)) {
            adapterHome = new AdapterHome(getActivity(), arrayListPost, Callback.TAG_SERIES);
            recyclerView.setAdapter(adapterHome);
            progressBar.setVisibility(View.GONE);
            loadMoreButton.setVisibility(View.VISIBLE);
        } else {
            adapterHome.notifyItemInserted(arrayListPost.size()-1);
        }
    }

    public void addSliderList(List<ItemPostHome> arrayListPostAll,
                              List<ItemSeries> arrayListSeries) {
        try {
            ItemPostHome item = new ItemPostHome("0", "slider", "slider");
            ArrayList<ItemSeries> arrayList = new ArrayList<>();
            int limitMovies = Math.min(SLIDER_ITEMS_LIMIT, arrayListSeries.size());
            for (int j = 0; j < limitMovies; j++) {
                arrayList.add(arrayListSeries.get(j));
            }
            item.setArrayListSeries(arrayList);
            arrayListPostAll.add(item);
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "Error addSliderList", e);
        }
    }

    public void addSeriesList(List<ItemCat> arrayListCat,
                              List<ItemPostHome> arrayListPostAll,
                              List<ItemSeries> arrayListSeries) {
        if (arrayListCat == null){
            return;
        }
        for (int i = 0; i < arrayListCat.size(); i++) {
            String categoryId = arrayListCat.get(i).getId();
            ItemPostHome itemPost = new ItemPostHome(
                    categoryId, arrayListCat.get(i).getName(), "data"
            );
            try {
                ArrayList<ItemSeries> arrayList = new ArrayList<>();
                List<ItemSeries> filteredList = filterByCategoryId(arrayListSeries, categoryId);
                if (!filteredList.isEmpty()) {
                    int limit = Math.min(CATEGORY_ITEMS_LIMIT, filteredList.size());
                    for (int j = 0; j < limit; j++) {
                        arrayList.add(filteredList.get(j));
                    }
                }
                itemPost.setArrayListSeries(arrayList);
            } catch (Exception e) {
                ApplicationUtil.log(TAG, "Error addSeriesList", e);
            }
            arrayListPostAll.add(itemPost);
        }
    }

    @NonNull
    public static List<ItemSeries> filterByCategoryId(List<ItemSeries> itemList, String categoryId) {
        List<ItemSeries> arrayList = new ArrayList<>();
        if (itemList == null) {
            return arrayList;
        }
        for (ItemSeries item : itemList) {
            if (item.getCatID().equals(categoryId)) {
                arrayList.add(item);
            }
        }
        return arrayList;
    }
}
