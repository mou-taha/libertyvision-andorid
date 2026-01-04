package nemosofts.streambox.adapter;

import android.content.Context;
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
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Picasso;

import java.util.List;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemChannel;

public class AdapterChannel extends RecyclerView.Adapter<AdapterChannel.MyViewHolder> {

    private final List<ItemChannel> arrayList;
    private final RecyclerItemClickListener listener;
    private final int columnWidth;

    public AdapterChannel(Context context,
                          List<ItemChannel> arrayList,
                          RecyclerItemClickListener listener) {
        this.arrayList = arrayList;
        this.listener = listener;
        boolean isTvBox  = DeviceUtils.isTvBox(context);
        columnWidth = DeviceUtils.getColumnWidth(context, isTvBox ? 8 : 7, 0);
        setHasStableIds(true);
    }

    @NonNull
    @Override
    public MyViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View  itemView = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.row_list_live, parent, false);
        return new MyViewHolder(itemView, columnWidth);
    }

    @Override
    public void onBindViewHolder(@NonNull MyViewHolder holder, int position) {
        ItemChannel item = arrayList.get(position);

        if (item.getName().isEmpty()){
            holder.title.setVisibility(View.GONE);
        } else {
            holder.title.setText(item.getName());
        }

        holder.linearLayout.setVisibility(View.GONE);

        try {
            String imageUrl = item.getStreamIcon();
            if (imageUrl == null || imageUrl.isEmpty()) {
                holder.logo.setImageResource(R.drawable.placeholder_live);
            } else {
                Picasso.get()
                        .load(imageUrl)
                        .priority(Picasso.Priority.NORMAL)
                        .placeholder(R.drawable.placeholder_live)
                        .error(R.drawable.placeholder_live)
                        .into(holder.logo);
            }
        } catch (Exception e) {
            holder.logo.setImageResource(R.drawable.placeholder_live);
        }

        holder.vw.setOnClickListener(v -> listener.onClickListener(
                arrayList.get(holder.getAbsoluteAdapterPosition()),
                holder.getAbsoluteAdapterPosition())
        );
    }

    public static class MyViewHolder extends RecyclerView.ViewHolder {

        private final View vw;
        private final ImageViewRound logo;
        private final TextView title;
        private final LinearLayout linearLayout;

        public MyViewHolder(View view, int columnWidth) {
            super(view);
            vw = view.findViewById(R.id.fd_movie_card);
            logo = view.findViewById(R.id.iv_movie);
            title = view.findViewById(R.id.tv_movie_title);
            linearLayout = view.findViewById(R.id.ll_card_star);

            RelativeLayout.LayoutParams params = new RelativeLayout.LayoutParams(columnWidth, columnWidth);
            logo.setScaleType(ImageView.ScaleType.CENTER_CROP);
            logo.setLayoutParams(params);
            vw.setLayoutParams(params);
        }
    }

    @Override
    public int getItemCount() {
        return arrayList.size();
    }

    @Override
    public long getItemId(int position) {
        return position;
    }

    public interface RecyclerItemClickListener{
        void onClickListener(ItemChannel itemChannel, int position);
    }
}