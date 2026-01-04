package nemosofts.streambox.adapter;

import android.content.Context;
import android.content.res.ColorStateList;
import android.graphics.Bitmap;
import android.graphics.drawable.Drawable;
import android.util.Log;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.nemosofts.material.ImageViewRound;

import androidx.nemosofts.utils.DeviceUtils;
import androidx.palette.graphics.Palette;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;
import com.squareup.picasso.Target;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemMovies;
import nemosofts.streambox.utils.ApplicationUtil;

public class AdapterBannerMovies extends RecyclerView.Adapter<AdapterBannerMovies.MyViewHolder> {

    private static final String TAG = "AdapterMovieBanner";
    private final List<ItemMovies> arrayList;
    private final int columnWidth;
    private final int columnHeight;
    private final RecyclerItemClickListener listener;

    public AdapterBannerMovies(Context context, List<ItemMovies> arrayList, RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
        columnWidth = DeviceUtils.getColumnWidth( context,2, 0);
        columnHeight = columnWidth / 3;
        setHasStableIds(true);
    }

    public static class MyViewHolder extends RecyclerView.ViewHolder {

        private final View vw;
        private final ImageViewRound poster;
        private final ImageViewRound posterFull;
        private final TextView rating;
        private final TextView title;

        private final LinearLayout llStar;

        public MyViewHolder(View view) {
            super(view);
            vw = view.findViewById(R.id.fd_movie_card);
            poster = view.findViewById(R.id.iv_movie);
            posterFull = view.findViewById(R.id.iv_home_post);
            title = view.findViewById(R.id.tv_movie_title);

            llStar = view.findViewById(R.id.ll_card_star);
            rating = view.findViewById(R.id.tv_movie_rating);
        }
    }

    @NonNull
    @Override
    public MyViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View itemView = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.row_list_movie_banner, parent, false);
        return new MyViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(@NonNull MyViewHolder holder, int position) {

        ItemMovies item = arrayList.get(position);

        holder.poster.setScaleType(ImageView.ScaleType.CENTER_CROP);
        holder.poster.setLayoutParams(new RelativeLayout.LayoutParams(columnWidth, (int) (columnHeight * 1.10)));
        holder.vw.setLayoutParams(new RelativeLayout.LayoutParams(columnWidth, (int) (columnHeight * 1.10)));

        holder.title.setText(item.getName());

        if (!item.getRating().isEmpty() && item.getRating().equals("0")){
            holder.llStar.setVisibility(View.GONE);
        } else {
            holder.rating.setText(item.getRating());
        }

        try {
            String imageUrl = item.getStreamIcon();
            if (imageUrl == null || imageUrl.isEmpty()) {
                holder.poster.setImageResource(R.drawable.placeholder_vertical);
                holder.posterFull.setImageResource(R.drawable.placeholder_vertical);
            } else {
                Picasso.get()
                        .load(imageUrl)
                        .placeholder(R.drawable.placeholder_vertical)
                        .error(R.drawable.placeholder_vertical)
                        .into(holder.poster);
                Picasso.get()
                        .load(imageUrl)
                        .placeholder(R.drawable.placeholder_vertical)
                        .error(R.drawable.placeholder_vertical)
                        .into(holder.posterFull);

                setupPalette(imageUrl, holder);
            }
        } catch (Exception e) {
            ApplicationUtil.log("Adapter", "Error glide load", e);
        }

        holder.vw.setOnClickListener(v -> listener.onClickListener(
                arrayList.get(holder.getAbsoluteAdapterPosition()),
                holder.getAbsoluteAdapterPosition())
        );
    }

    private void setupPalette(String imageUrl, MyViewHolder holder) {
        Picasso.get()
                .load(imageUrl)
                .centerCrop()
                .resize(100, 100)
                .into(new Target() {
                    @Override
                    public void onBitmapLoaded(Bitmap bitmap, Picasso.LoadedFrom from) {
                        if (bitmap == null){
                            return;
                        }
                        try {
                            Palette.from(bitmap).generate(palette -> {
                                if (palette == null || palette.getVibrantSwatch() == null){
                                    return;
                                }
                                Palette.Swatch textSwatch = palette.getVibrantSwatch();
                                try {
                                    holder.title.setTextColor(ColorStateList.valueOf(textSwatch.getRgb()));
                                } catch (Exception e) {
                                    Log.e(TAG, "Error in color", e);
                                }
                            });
                        } catch (Exception e) {
                            Log.e(TAG, "Error in color", e);
                        }

                    }
                    @Override
                    public void onBitmapFailed(Exception e, Drawable errorDrawable) {
                        // this method is empty
                    }
                    @Override
                    public void onPrepareLoad(Drawable placeHolderDrawable) {
                        // this method is empty
                    }
                });
    }

    @Override
    public long getItemId(int id) {
        return id;
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemMovies itemMovies, int position);
    }
}