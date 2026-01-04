package nemosofts.streambox.executor;

import android.content.Context;

import android.util.JsonReader;

import java.io.IOException;
import java.io.StringReader;

import nemosofts.streambox.callback.Callback;
import nemosofts.streambox.interfaces.DataListener;
import nemosofts.streambox.utils.ApplicationUtil;
import nemosofts.streambox.utils.AsyncTaskExecutor;
import nemosofts.streambox.utils.helper.JSHelper;
import nemosofts.streambox.utils.helper.SPHelper;

public class LoadData extends AsyncTaskExecutor<String, String, String> {

    private static final String TAG = "LoadData";
    private final JSHelper jsHelper;
    private final SPHelper spHelper;
    private final DataListener listener;

    public LoadData(Context ctx, DataListener listener) {
        this.listener = listener;
        spHelper = new SPHelper(ctx);
        jsHelper = new JSHelper(ctx);
    }

    @Override
    protected void onPreExecute() {
        listener.onStart();
        super.onPreExecute();
    }

    @Override
    protected String doInBackground(String strings) {
        try {
            if (isUpdateNeeded()){
                jsHelper.setUpdateDate();
                updateSeriesData();
                updateMoviesData();
                updateLiveStreams();
                return "1";
            } else {
                return "2";
            }
        } catch (Exception e) {
            return "0";
        }
    }

    private boolean isUpdateNeeded() {
        if (jsHelper.getUpdateDate().isEmpty()) {
            jsHelper.setUpdateDate();
            return true;
        }
        // Hours Check
        return Boolean.TRUE.equals(ApplicationUtil.calculateUpdateHours(
                jsHelper.getUpdateDate(), spHelper.getAutoUpdate())
        );
    }

    private void updateLiveStreams() {
        try {
            if (!spHelper.getCurrent(Callback.TAG_TV).isEmpty()){
                String currentLive = spHelper.getCurrent(Callback.TAG_TV);
                if (spHelper.getIsUpdateLive() && !currentLive.isEmpty()) {
                    String jsonLive = ApplicationUtil.responsePost(spHelper.getAPI(),
                            ApplicationUtil.getAPIRequest("get_live_streams",
                                    spHelper.getUserName(), spHelper.getPassword())
                    );
                    if (!jsonLive.isEmpty()) {
                        int totalLive = countArrayItems(jsonLive);
                        if (totalLive != 0 && totalLive != jsHelper.getLiveSize()) {
                            jsHelper.setLiveSize(totalLive);
                            jsHelper.addToLiveData(jsonLive);
                            Callback.setSuccessLive("1");
                        }
                    }
                }
            }
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "get_live_streams", e);
        }
    }

    private void updateMoviesData() {
        try {
            if (!spHelper.getCurrent(Callback.TAG_MOVIE).isEmpty()){
                String currentMovies = spHelper.getCurrent(Callback.TAG_MOVIE);
                if (spHelper.getIsUpdateMovies() && !currentMovies.isEmpty()) {
                    String jsonMovies = ApplicationUtil.responsePost(spHelper.getAPI(),
                            ApplicationUtil.getAPIRequest("get_vod_streams",
                                    spHelper.getUserName(), spHelper.getPassword())
                    );
                    if (!jsonMovies.isEmpty()) {
                        int totalMovies = countArrayItems(jsonMovies);
                        if (totalMovies != 0 && totalMovies != jsHelper.getMoviesSize()) {
                            jsHelper.setMovieSize(totalMovies);
                            jsHelper.addToMovieData(jsonMovies);
                            Callback.setSuccessMovies("1");
                        }
                    }
                }
            }
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "get_vod_streams", e);
        }
    }

    private void updateSeriesData() {
        try {
            if (!spHelper.getCurrent(Callback.TAG_SERIES).isEmpty()){
                String currentSeries = spHelper.getCurrent(Callback.TAG_SERIES);
                if (spHelper.getIsUpdateSeries() && !currentSeries.isEmpty()) {
                    String jsonSeries = ApplicationUtil.responsePost(spHelper.getAPI(),
                            ApplicationUtil.getAPIRequest("get_series",
                                    spHelper.getUserName(), spHelper.getPassword())
                    );
                    if (!jsonSeries.isEmpty()) {
                        int totalSeries = countArrayItems(jsonSeries);
                        if (totalSeries != 0 && totalSeries != jsHelper.getSeriesSize()) {
                            jsHelper.setSeriesSize(totalSeries);
                            jsHelper.addToSeriesData(jsonSeries);
                            Callback.setSuccessSeries("1");
                        }
                    }
                }
            }
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "get_series", e);
        }
    }

    @Override
    protected void onPostExecute(String s) {
        listener.onEnd(s);
        this.shutDown();
    }

    private int countArrayItems(String json) {
        if (json == null || json.isEmpty()) {
            return 0;
        }

        try (JsonReader reader = new JsonReader(new StringReader(json))) {

            reader.beginArray();

            int count = 0;
            while (reader.hasNext()) {
                reader.skipValue();
                count++;
            }

            reader.endArray();
            return count;

        } catch (IOException | IllegalStateException e) {
            return 0;
        }
    }
}
