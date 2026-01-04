package nemosofts.streambox.utils.player;

import android.content.Context;
import android.util.Pair;

import androidx.annotation.NonNull;
import androidx.annotation.OptIn;
import androidx.appcompat.app.AlertDialog;
import androidx.media3.common.C;
import androidx.media3.common.Format;
import androidx.media3.common.TrackSelectionOverride;
import androidx.media3.common.TrackSelectionParameters;
import androidx.media3.common.Tracks;
import androidx.media3.common.util.UnstableApi;
import androidx.media3.exoplayer.ExoPlayer;

import java.util.ArrayList;
import java.util.List;
import java.util.Locale;

import nemosofts.streambox.R;
import nemosofts.streambox.utils.ApplicationUtil;

public class VideoTracksUtils {

    public interface VideoTrackSelectionListener {
        void onVideoTrackSelected(int selectedIndex);
    }

    public static void showVideoTracksDialog(Context context,
                                             ExoPlayer player,
                                             VideoTrackSelectionListener listener) {
        if (player == null) {
            return;
        }

        Tracks tracks = player.getCurrentTracks();
        Pair<List<CharSequence>, Integer> trackData = prepareTrackList(context, tracks);

        new AlertDialog.Builder(context)
                .setTitle(context.getString(R.string.select_video_track))
                .setSingleChoiceItems(
                        trackData.first.toArray(new CharSequence[0]),
                        trackData.second,
                        (dialog, which) -> {
                            dialog.dismiss();
                            if (listener != null) {
                                listener.onVideoTrackSelected(which - 1); // Adjust for "Auto"
                            }
                        })
                .setNegativeButton(android.R.string.cancel, null)
                .show();
    }

    public static void selectVideoTrack(@NonNull ExoPlayer player, int selectedIndex) {
        TrackSelectionParameters.Builder builder = player.getTrackSelectionParameters().buildUpon();

        if (selectedIndex == -1) {
            builder.clearOverridesOfType(C.TRACK_TYPE_VIDEO);
            player.setTrackSelectionParameters(builder.build());
            return;
        }

        Tracks tracks = player.getCurrentTracks();
        int currentIndex = 0;
        for (Tracks.Group group : tracks.getGroups()) {
            if (group.getType() == C.TRACK_TYPE_VIDEO) {
                for (int i = 0; i < group.length; i++) {
                    if (currentIndex == selectedIndex) {
                        TrackSelectionOverride override = new TrackSelectionOverride(group.getMediaTrackGroup(), i);
                        builder.setOverrideForType(override);
                        player.setTrackSelectionParameters(builder.build());
                        return;
                    }
                    currentIndex++;
                }
            }
        }
        player.setTrackSelectionParameters(builder.build());
    }

    @NonNull
    public static Pair<List<CharSequence>, Integer> prepareTrackList(@NonNull Context context,
                                                                     @NonNull Tracks tracks) {
        List<CharSequence> trackList = new ArrayList<>();
        int selectedIndex = 0; // Default is "Auto"

        trackList.add(context.getString(R.string.track_auto));

        int currentIndex = 0;
        for (Tracks.Group group : tracks.getGroups()) {
            if (group.getType() != C.TRACK_TYPE_VIDEO) continue;

            for (int i = 0; i < group.length; i++) {
                Format format = group.getTrackFormat(i);
                String name = buildTrackLabel(context, format);
                trackList.add(name);

                if (group.isTrackSelected(i)) {
                    selectedIndex = currentIndex + 1; // +1 because of "Auto"
                }

                currentIndex++;
            }
        }

        return new Pair<>(trackList, selectedIndex);
    }

    @OptIn(markerClass = UnstableApi.class)
    private static String buildTrackLabel(Context context, Format format) {
        if (format == null) {
            return context.getString(R.string.track_unknown_quality);
        }

        StringBuilder builder = new StringBuilder();

        if (format.height != Format.NO_VALUE) {
            builder.append(format.height).append("p");
        }

        if (format.frameRate != Format.NO_VALUE) {
            builder.append(" @ ").append((int) format.frameRate).append("fps");
        }

        if (format.bitrate != Format.NO_VALUE) {
            builder.append(" - ").append(String.format(Locale.US, "%.2f Mbps", format.bitrate / 1_000_000f));
        }

        return ApplicationUtil.isEmpty(builder, context.getString(R.string.track_unknown_quality));
    }
}