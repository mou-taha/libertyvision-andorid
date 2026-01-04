package nemosofts.streambox.adapter;

import android.content.Context;
import android.text.TextUtils;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.widget.ImageView;
import android.widget.LinearLayout;
import android.widget.ProgressBar;
import android.widget.RatingBar;
import android.widget.RelativeLayout;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.nemosofts.utils.FormatUtils;
import androidx.recyclerview.widget.RecyclerView;

import com.squareup.picasso.Callback;
import com.squareup.picasso.Picasso;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.Set;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemEpisodes;
import nemosofts.streambox.item.ItemVideoDownload;
import nemosofts.streambox.utils.ApplicationUtil;
import nemosofts.streambox.utils.helper.DBHelper;

public class AdapterEpisodes extends RecyclerView.Adapter<AdapterEpisodes.MyViewHolder> {

    private final Context context;
    private final List<ItemEpisodes> arrayList;
    private final RecyclerItemClickListener listener;
    private final String seriesCover;
    private final DBHelper dbHelper;
    private final boolean showDownloadControls;
    private final Map<String, Integer> downloadProgressMap = new HashMap<>();

    public AdapterEpisodes(Context ctx,
                           List<ItemEpisodes> arrayList,
                           String cover,
                           boolean showDownloadControls,
                           RecyclerItemClickListener listener) {
        this.context = ctx;
        this.arrayList = arrayList;
        this.listener = listener;
        this.seriesCover = cover;
        this.showDownloadControls = showDownloadControls;
        dbHelper = new DBHelper(ctx);
    }

    @NonNull
    @Override
    public MyViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        View itemView = LayoutInflater.from(parent.getContext())
                .inflate(R.layout.row_list_episodes, parent, false);
        return new MyViewHolder(itemView);
    }

    @Override
    public void onBindViewHolder(@NonNull MyViewHolder holder, int position) {
        ItemEpisodes item = arrayList.get(position);

        try {
            String imageUrl = item.getCoverBig();

            // Helper method to load seriesCover or fallback image
            Runnable loadSeriesCoverOrFallback = () -> {
                if (seriesCover == null || seriesCover.isEmpty()) {
                    holder.poster.setImageResource(R.drawable.placeholder_horizontal);
                } else {
                    Picasso.get()
                            .load(seriesCover)
                            .resize(450, 300)
                            .centerCrop()
                            .placeholder(R.drawable.placeholder_horizontal)
                            .error(R.drawable.placeholder_horizontal)
                            .into(holder.poster);
                }
            };

            if (TextUtils.isEmpty(imageUrl)) {
                loadSeriesCoverOrFallback.run();
            } else {
                Picasso.get()
                        .load(imageUrl)
                        .resize(450, 300)
                        .centerCrop()
                        .placeholder(R.drawable.placeholder_horizontal)
                        .error(R.drawable.placeholder_horizontal)
                        .into(holder.poster, new Callback() {
                            @Override
                            public void onSuccess() {
                                // Optional: log success or leave empty
                            }

                            @Override
                            public void onError(Exception e) {
                                loadSeriesCoverOrFallback.run();
                            }
                        });
            }
        } catch (Exception e) {
            ApplicationUtil.log("Adapter", "Error loading image with Picasso", e);
        }

        holder.title.setText(item.getTitle());

        try {
            long seek = dbHelper.getSeekFull(DBHelper.TABLE_SEEK_EPISODES,
                    item.getId(), item.getTitle()
            );
            if (seek > 0){
                holder.pb.setVisibility(View.VISIBLE);
                holder.pb.setProgress(Math.toIntExact(seek));
            } else {
                holder.pb.setVisibility(View.GONE);
            }
        } catch (Exception e) {
            holder.pb.setVisibility(View.GONE);
        }

        holder.rb.setRating(getRating(item));

        try {
            holder.duration.setText(FormatUtils.formatTimeDuration(item.getDuration()));
        } catch (Exception e) {
            holder.duration.setText("0");
        }

        holder.relativeLayout.setOnClickListener(v -> {
            if (listener != null) {
                int adapterPosition = holder.getAbsoluteAdapterPosition();
                if (adapterPosition != RecyclerView.NO_POSITION) {
                    listener.onClickListener(arrayList.get(adapterPosition), adapterPosition);
                }
            }
        });
        holder.relativeLayout.setOnLongClickListener(v -> {
            if (showDownloadControls && listener != null) {
                int adapterPosition = holder.getAbsoluteAdapterPosition();
                if (adapterPosition != RecyclerView.NO_POSITION) {
                    listener.onDownloadClick(arrayList.get(adapterPosition), adapterPosition);
                    return true;
                }
            }
            return false;
        });

        bindDownloadState(holder, item);
    }

    private void bindDownloadState(@NonNull MyViewHolder holder, ItemEpisodes item) {
        if (!showDownloadControls) {
            holder.downloadContainer.setVisibility(View.GONE);
            holder.downloadStatus.setText("");
            holder.downloadIcon.setOnClickListener(null);
            holder.downloadCancelIcon.setVisibility(View.GONE);
            holder.downloadCancelIcon.setOnClickListener(null);
            return;
        }

        holder.downloadContainer.setVisibility(View.VISIBLE);
        holder.downloadIcon.setOnClickListener(null);

        boolean isDownloaded = dbHelper.checkDownload(
                DBHelper.TABLE_DOWNLOAD_EPISODES,
                item.getId(),
                ApplicationUtil.containerExtension(item.getContainerExtension())
        );

        Integer progress = downloadProgressMap.get(item.getId());

        if (progress != null) {
            holder.downloadStatus.setText(formatProgressText(progress));
            holder.downloadIcon.setImageResource(R.drawable.iv_downloading);
            holder.downloadIcon.setEnabled(false);
            holder.downloadIcon.setAlpha(0.5f);
            holder.downloadCancelIcon.setVisibility(View.VISIBLE);
            holder.downloadCancelIcon.setAlpha(1f);
            holder.downloadCancelIcon.setEnabled(true);
            holder.downloadCancelIcon.setOnClickListener(v -> {
                if (listener != null) {
                    int adapterPosition = holder.getAbsoluteAdapterPosition();
                    if (adapterPosition != RecyclerView.NO_POSITION) {
                        listener.onCancelDownloadClick(arrayList.get(adapterPosition), adapterPosition);
                    }
                }
            });
        } else if (isDownloaded) {
            holder.downloadStatus.setText(context.getString(R.string.downloaded));
            holder.downloadIcon.setImageResource(R.drawable.ic_check);
            holder.downloadIcon.setEnabled(false);
            holder.downloadIcon.setAlpha(0.5f);
            holder.downloadCancelIcon.setVisibility(View.GONE);
            holder.downloadCancelIcon.setOnClickListener(null);
        } else {
            holder.downloadStatus.setText(context.getString(R.string.download));
            holder.downloadIcon.setImageResource(R.drawable.iv_downloading);
            holder.downloadIcon.setEnabled(true);
            holder.downloadIcon.setAlpha(1f);
            holder.downloadIcon.setOnClickListener(v -> {
                if (listener != null) {
                    int adapterPosition = holder.getAbsoluteAdapterPosition();
                    if (adapterPosition != RecyclerView.NO_POSITION) {
                        listener.onDownloadClick(arrayList.get(adapterPosition), adapterPosition);
                    }
                }
            });
            holder.downloadCancelIcon.setVisibility(View.GONE);
            holder.downloadCancelIcon.setOnClickListener(null);
        }
    }

    @NonNull
    private String formatProgressText(int progress) {
        int safeProgress = Math.max(0, Math.min(100, progress));
        return String.format(Locale.getDefault(), "%s %d%%",
                context.getString(R.string.downloading),
                safeProgress
        );
    }

    public void updateDownloadQueue(List<ItemVideoDownload> downloads) {
        if (!showDownloadControls) {
            return;
        }

        Set<String> activeIds = new HashSet<>();
        if (downloads != null) {
            for (ItemVideoDownload download : downloads) {
                if (download == null) {
                    continue;
                }
                if (!DBHelper.TABLE_DOWNLOAD_EPISODES.equals(download.getDownloadTable())) {
                    continue;
                }
                activeIds.add(download.getStreamID());
                Integer currentProgress = downloadProgressMap.get(download.getStreamID());
                int newProgress = download.getProgress();
                if (currentProgress == null || currentProgress != newProgress) {
                    downloadProgressMap.put(download.getStreamID(), newProgress);
                    notifyEpisodeChanged(download.getStreamID());
                }
            }
        }

        Iterator<Map.Entry<String, Integer>> iterator = downloadProgressMap.entrySet().iterator();
        while (iterator.hasNext()) {
            Map.Entry<String, Integer> entry = iterator.next();
            if (!activeIds.contains(entry.getKey())) {
                iterator.remove();
                notifyEpisodeChanged(entry.getKey());
            }
        }
    }

    private void notifyEpisodeChanged(String episodeId) {
        int position = getEpisodePosition(episodeId);
        if (position != -1) {
            notifyItemChanged(position);
        }
    }

    private int getEpisodePosition(String episodeId) {
        for (int i = 0; i < arrayList.size(); i++) {
            if (arrayList.get(i).getId().equals(episodeId)) {
                return i;
            }
        }
        return -1;
    }

    public static class MyViewHolder extends RecyclerView.ViewHolder {

        private final ImageView poster;
        private final TextView title;
        private final TextView duration;
        private final RatingBar rb;
        private final ProgressBar pb;
        private final RelativeLayout relativeLayout;
        private final LinearLayout downloadContainer;
        private final TextView downloadStatus;
        private final ImageView downloadIcon;
        private final ImageView downloadCancelIcon;

        private MyViewHolder(View view) {
            super(view);
            poster = view.findViewById(R.id.iv_episodes);
            title = view.findViewById(R.id.tv_episodes);
            rb = view.findViewById(R.id.rb_episodes_list);
            duration = view.findViewById(R.id.tv_duration);
            relativeLayout = view.findViewById(R.id.rl_episodes);
            pb = view.findViewById(R.id.pr_episodes);
            downloadContainer = view.findViewById(R.id.ll_download_status);
            downloadStatus = view.findViewById(R.id.tv_download_status);
            downloadIcon = view.findViewById(R.id.iv_episode_download);
            downloadCancelIcon = view.findViewById(R.id.iv_episode_cancel);
        }
    }

    private float getRating(ItemEpisodes item) {
        try {
            // Adding null check before parsing the rating
            String ratingString = item.getRating().isEmpty() ? "0" : item.getRating();
            double newRating = 0.0; // default value
            if (ratingString != null && !ratingString.isEmpty()) {
                newRating = convertToFiveRating(Double.parseDouble(ratingString));
            }
            return (float) newRating;
        } catch (Exception e) {
            return 0;
        }
    }

    public static double convertToFiveRating(double oldRating) {
        return (oldRating - 1) * 4 / 9 + 1;
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
        void onClickListener(ItemEpisodes itemEpisodes, int position);
        void onDownloadClick(ItemEpisodes itemEpisodes, int position);
        void onCancelDownloadClick(ItemEpisodes itemEpisodes, int position);
    }
}
