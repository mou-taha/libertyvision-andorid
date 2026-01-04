package nemosofts.streambox.adapter;

import android.content.Context;
import android.content.Intent;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.nemosofts.material.IconTextView;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.activity.DetailsMovieActivity;
import nemosofts.streambox.activity.DetailsSeriesActivity;
import nemosofts.streambox.activity.MovieActivity;
import nemosofts.streambox.activity.SeriesActivity;
import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.item.ItemMovies;
import nemosofts.streambox.item.ItemPostHome;
import nemosofts.streambox.item.ItemSeries;

public class AdapterHome extends RecyclerView.Adapter<RecyclerView.ViewHolder> {

    private final Context context;
    private final List<ItemPostHome> arrayList;
    private final String type;

    private static final int ITEMS_LIMIT = 7;

    private static final int VIEW_PROG = 0;
    private static final int VIEW_BANNER = 1;
    private static final int VIEW_POST = 2;


    private static final String TAG_CAT_ID = "cat_id";
    private static final String TAG_PAGE_TYPE = "pageType";

    public AdapterHome(Context context, List<ItemPostHome> arrayList, String type) {
        this.context = context;
        this.arrayList = arrayList;
        this.type= type;

    }

    private class BannerHolder extends RecyclerView.ViewHolder {

        RecyclerView rv;
        AdapterBannerMovies adapterBannerMovies;
        AdapterBannerSeries adapterBannerSeries;

        BannerHolder(View view) {
            super(view);
            rv = view.findViewById(R.id.rv_banner);
            GridLayoutManager grid = new GridLayoutManager(context, 1);
            grid.setSpanCount(2);
            rv.setLayoutManager(grid);
            rv.setItemAnimator(new DefaultItemAnimator());
        }
    }

    private class PostHolder extends RecyclerView.ViewHolder {

        RecyclerView rv;
        AdapterHomeMovies adapterHomeMovies;
        AdapterHomeSeries adapterHomeSeries;
        TextView title;
        IconTextView viewAll;

        PostHolder(View view) {
            super(view);
            rv = view.findViewById(R.id.rv_home_cat);
            title = view.findViewById(R.id.tv_home_title);
            viewAll = view.findViewById(R.id.btn_view_all);
            GridLayoutManager grid = new GridLayoutManager(context, 1);
            grid.setSpanCount(7);
            rv.setLayoutManager(grid);
            rv.setItemAnimator(new DefaultItemAnimator());
        }
    }

    private static class ProgressViewHolder extends RecyclerView.ViewHolder {
        private ProgressViewHolder(View v) {
            super(v);
        }
    }

    @NonNull
    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        if (viewType == VIEW_BANNER) {
            View itemView = LayoutInflater.from(parent.getContext())
                    .inflate(R.layout.layout_home_ui_banner, parent, false);
            return new BannerHolder(itemView);
        } else if (viewType == VIEW_POST) {
            View itemView = LayoutInflater.from(parent.getContext())
                    .inflate(R.layout.layout_home_ui_list, parent, false);
            return new PostHolder(itemView);
        } else {
            View itemView = LayoutInflater.from(parent.getContext())
                    .inflate(R.layout.row_progressbar, parent, false);
            return new ProgressViewHolder(itemView);
        }
    }

    @Override
    public void onBindViewHolder(@NonNull RecyclerView.ViewHolder holder, int position) {
        if (holder instanceof BannerHolder bannerHolder) {
            bindBannerHolder(bannerHolder, position);
        }
        else if (holder instanceof PostHolder postHolder) {

            if (type.equals(Callback.TAG_MOVIE)){
                postHolder.title.setText(arrayList.get(holder.getAbsoluteAdapterPosition()).getTitle());

                postHolder.adapterHomeMovies = new AdapterHomeMovies(
                        context,
                        arrayList.get(holder.getAbsoluteAdapterPosition()).getArrayListMovies(),
                        (itemMovies, position2) ->
                                launchMovieDetails(holder.getAbsoluteAdapterPosition(), position2)
                );
                postHolder.rv.setAdapter(postHolder.adapterHomeMovies);

                postHolder.viewAll.setOnClickListener(v -> {
                    Intent intent = new Intent(context, MovieActivity.class);
                    intent.putExtra(TAG_PAGE_TYPE, MovieActivity.TAG_TYPE_ONLINE);
                    intent.putExtra(TAG_CAT_ID, arrayList.get(holder.getAbsoluteAdapterPosition()).getCatID());
                    context.startActivity(intent);
                });

                if (arrayList.get(holder.getAbsoluteAdapterPosition()).getArrayListMovies().size() >= ITEMS_LIMIT){
                    postHolder.viewAll.setVisibility(View.VISIBLE);
                } else {
                    postHolder.viewAll.setVisibility(View.GONE);
                }
            } else if (type.equals(Callback.TAG_SERIES)){
                postHolder.title.setText(arrayList.get(holder.getAbsoluteAdapterPosition()).getTitle());

                postHolder.adapterHomeSeries = new AdapterHomeSeries(
                        context,
                        arrayList.get(holder.getAbsoluteAdapterPosition()).getArrayListSeries(),
                        (itemSeries, position4) ->
                                launchSeriesDetails(holder.getAbsoluteAdapterPosition(), position4)
                );
                postHolder.rv.setAdapter(postHolder.adapterHomeSeries);

                postHolder.viewAll.setOnClickListener(v -> {
                    Intent intent = new Intent(context, SeriesActivity.class);
                    intent.putExtra(TAG_CAT_ID, arrayList.get(holder.getAbsoluteAdapterPosition()).getCatID());
                    context.startActivity(intent);
                });

                if (arrayList.get(holder.getAbsoluteAdapterPosition()).getArrayListSeries().size() >= ITEMS_LIMIT){
                    postHolder.viewAll.setVisibility(View.VISIBLE);
                } else {
                    postHolder.viewAll.setVisibility(View.GONE);
                }
            }
        }
    }

    private void bindBannerHolder(BannerHolder holder, int position) {
        if (type.equals(Callback.TAG_MOVIE)) {
            setupMovieBanner(holder, position);
        } else if (type.equals(Callback.TAG_SERIES)) {
            setupSeriesBanner(holder, position);
        }
    }

    private void setupMovieBanner(BannerHolder holder, int position) {
        if (holder == null){
            return;
        }
        holder.adapterBannerMovies = new AdapterBannerMovies(
                context,
                arrayList.get(position).getArrayListMovies(),
                (itemMovies, pos) -> launchMovieDetails(position, pos)
        );
        holder.rv.setAdapter(holder.adapterBannerMovies);
    }

    private void setupSeriesBanner(BannerHolder holder, int position) {
        if (holder == null){
            return;
        }
        holder.adapterBannerSeries = new AdapterBannerSeries(
                context,
                arrayList.get(position).getArrayListSeries(),
                (itemSeries, pos) -> launchSeriesDetails(position, pos)
        );
        holder.rv.setAdapter(holder.adapterBannerSeries);
    }

    private void launchMovieDetails(int holderPosition, int itemPosition) {
        ItemMovies item = arrayList.get(holderPosition).getArrayListMovies().get(itemPosition);
        Intent intent = new Intent(context, DetailsMovieActivity.class);
        intent.putExtra("stream_id", item.getStreamID());
        intent.putExtra("stream_name", item.getName());
        intent.putExtra("stream_icon", item.getStreamIcon());
        intent.putExtra("stream_rating", item.getRating());
        context.startActivity(intent);
    }

    private void launchSeriesDetails(int holderPosition, int itemPosition) {
        ItemSeries item = arrayList.get(holderPosition).getArrayListSeries().get(itemPosition);
        Intent intent = new Intent(context, DetailsSeriesActivity.class);
        intent.putExtra("series_id", item.getSeriesID());
        intent.putExtra("series_name", item.getName());
        intent.putExtra("series_rating", item.getRating());
        intent.putExtra("series_cover", item.getCover());
        context.startActivity(intent);
    }

    @Override
    public long getItemId(int id) {
        return id;
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    @Override
    public int getItemViewType(int position) {
        return switch (arrayList.get(position).getType()) {
            case "slider" -> VIEW_BANNER;
            case "data" -> VIEW_POST;
            default -> VIEW_PROG;
        };
    }
}