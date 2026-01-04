package nemosofts.streambox.activity;

import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.app.PendingIntent;
import android.app.Service;
import android.content.Context;
import android.content.Intent;
import android.content.pm.ServiceInfo;
import android.os.Build;
import android.os.Handler;
import android.os.IBinder;
import android.os.Looper;
import android.os.Message;
import android.util.Log;
import android.widget.RemoteViews;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.app.NotificationCompat;
import androidx.nemosofts.coreprogress.ProgressHelper;
import androidx.nemosofts.coreprogress.ProgressUIListener;

import java.io.Closeable;
import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import nemosofts.streambox.R;
import nemosofts.streambox.item.ItemVideoDownload;
import nemosofts.streambox.utils.ApplicationUtil;
import nemosofts.streambox.utils.encrypter.Encrypt;
import okhttp3.Call;
import okhttp3.Callback;
import okhttp3.OkHttpClient;
import okhttp3.Request;
import okhttp3.Response;
import okhttp3.ResponseBody;
import okio.BufferedSource;

public class DownloadService extends Service {

    private static final String TAG = "DownloadService";
    private NotificationCompat.Builder myNotify;
    private RemoteViews rv;
    private OkHttpClient client;
    public static final String ACTION_STOP = "com.mydownload.action.STOP";
    public static final String ACTION_START = "com.mydownload.action.START";
    public static final String ACTION_ADD = "com.mydownload.action.ADD";
    public static final String ACTION_CANCEL_SINGLE = "com.mydownload.action.CANCEL_SINGLE";
    private static final String CANCEL_TAG = "c_tag";
    private NotificationManager mNotificationManager;

    private static DownloadService instance;

    private Encrypt enc;
    private Boolean isDownloaded = false;
    private Thread thread;
    private Call call;
    private int count = 0;
    private static final List<String> arrayListName = new ArrayList<>();
    private static final List<String> arrayListContainer = new ArrayList<>();
    private static final List<String> arrayListFilePath = new ArrayList<>();
    private static final List<String> arrayListURL = new ArrayList<>();
    private static final List<ItemVideoDownload> arrayListVideo = new ArrayList<>();
    private static final int MY_NOTIFICATION_ID = 1002;
    private static final String DOWNLOAD_CHANNEL_ID = "download_ch_1";

    public static DownloadService getInstance() {
        if (instance == null) {
            instance = new DownloadService();
        }
        return instance;
    }

    public static Boolean isDownloading() {
        return !arrayListFilePath.isEmpty();
    }

    public static List<ItemVideoDownload> getArrayListVideo() {
        return arrayListVideo;
    }

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    private final Handler mHandler = new Handler(Looper.getMainLooper(), msg -> {
        switch (msg.what) {
            case 1:
                int progress = Integer.parseInt(msg.obj.toString());
                if (!arrayListVideo.isEmpty()) {
                    arrayListVideo.get(0).setProgress(progress);
                    rv.setProgressBar(R.id.progress, 100, progress, false);
                    mNotificationManager.notify(MY_NOTIFICATION_ID, myNotify.build());
                }
                break;
            case 0:
                if (!arrayListVideo.isEmpty()) {
                    rv.setTextViewText(R.id.nf_title, arrayListVideo.get(0).getName());
                    rv.setTextViewText(R.id.nf_percentage, count - (arrayListURL.size() - 1) + "/" + count + " " + getString(R.string.downloading));
                    mNotificationManager.notify(MY_NOTIFICATION_ID, myNotify.build());
                }
                break;
            case 2:
                try {
                    Thread.sleep(500);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    ApplicationUtil.log("Thread", "Thread was interrupted", e);
                }
                rv.setProgressBar(R.id.progress, 100, 100, false);
                rv.setTextViewText(R.id.nf_percentage, count + "/" + count + " " + getString(R.string.downloaded));
                mNotificationManager.notify(MY_NOTIFICATION_ID, myNotify.build());
                count = 0;
                break;
            default:
                break;
        }
        return false;
    });

    @Override
    public void onCreate() {
        super.onCreate();

        enc = Encrypt.getInstance();
        enc.init(this);

        mNotificationManager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
        createNotificationChannel();

        myNotify = new NotificationCompat.Builder(this, DOWNLOAD_CHANNEL_ID)
                .setSmallIcon(R.drawable.ic_file_download_not)
                .setTicker(getResources().getString(R.string.downloading))
                .setWhen(System.currentTimeMillis())
                .setOnlyAlertOnce(true)
                .setPriority(NotificationCompat.PRIORITY_LOW);

        rv = new RemoteViews(getPackageName(), R.layout.row_custom_notification);
        rv.setTextViewText(R.id.nf_title, getString(R.string.app_name));
        rv.setProgressBar(R.id.progress, 100, 0, false);
        rv.setTextViewText(R.id.nf_percentage, getResources().getString(R.string.downloading) + " " + "(0%)");

        Intent closeIntent = new Intent(this, DownloadService.class);
        closeIntent.setAction(ACTION_STOP);
        int flags = Build.VERSION.SDK_INT >= Build.VERSION_CODES.S
                ? PendingIntent.FLAG_UPDATE_CURRENT | PendingIntent.FLAG_IMMUTABLE
                : PendingIntent.FLAG_UPDATE_CURRENT;
        PendingIntent pCloseIntent = PendingIntent.getService(this, 0, closeIntent, flags);
        rv.setOnClickPendingIntent(R.id.iv_stop_download, pCloseIntent);

        myNotify.setCustomContentView(rv);

        startForegroundService();
    }

    private void createNotificationChannel() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
            CharSequence name = "Online Channel download";
            NotificationChannel mChannel = new NotificationChannel(DOWNLOAD_CHANNEL_ID,
                    name, NotificationManager.IMPORTANCE_LOW);
            mNotificationManager.createNotificationChannel(mChannel);
        }
    }

    private void startForegroundService() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.Q) {
            startForeground(MY_NOTIFICATION_ID, myNotify.build(),
                    ServiceInfo.FOREGROUND_SERVICE_TYPE_DATA_SYNC);
        } else {
            startForeground(MY_NOTIFICATION_ID, myNotify.build());
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        try {
            stopForeground();
            stop(null);
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "Error stopping service", e);
        }
    }

    private void stopForeground() {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.N) {
            stopForeground(STOP_FOREGROUND_REMOVE);
        } else {
            stopForeground(true);
        }
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId) {
        super.onStartCommand(intent, flags, startId);
        if (intent != null && intent.getAction() != null) {
            return switch (intent.getAction()) {
                case ACTION_START -> {
                    handleStartAction(intent);
                    yield START_NOT_STICKY;
                }
                case ACTION_STOP -> {
                    stop(intent);
                    yield START_STICKY;
                }
                case ACTION_ADD -> {
                    handleAddAction(intent);
                    yield START_REDELIVER_INTENT;
                }
                case ACTION_CANCEL_SINGLE -> {
                    handleCancelAction(intent);
                    yield START_NOT_STICKY;
                }
                default -> START_STICKY;
            };
        }
        return START_STICKY;
    }

    private void handleStartAction(@NonNull Intent intent) {
        ItemVideoDownload itemVideoDownload = getItemVideoDownload(intent);
        if (itemVideoDownload != null) {
            arrayListURL.add(intent.getStringExtra("downloadUrl"));
            arrayListFilePath.add(intent.getStringExtra("file_path"));
            arrayListName.add(intent.getStringExtra("file_name"));
            arrayListContainer.add(intent.getStringExtra("file_container"));
            arrayListVideo.add(itemVideoDownload);
            count++;
            init();
        }
    }

    @SuppressWarnings("deprecation")
    public static ItemVideoDownload getItemVideoDownload(Intent intent) {
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.TIRAMISU) {
            return intent.getSerializableExtra("item", ItemVideoDownload.class);
        } else {
            return (ItemVideoDownload) intent.getSerializableExtra("item");
        }
    }

    private void handleAddAction(@NonNull Intent intent) {
        ItemVideoDownload itemVideoDownload = getItemVideoDownload(intent);
        if (itemVideoDownload != null && !isVideoAlreadyAdded(itemVideoDownload)) {
            count++;
            arrayListURL.add(intent.getStringExtra("downloadUrl"));
            arrayListFilePath.add(intent.getStringExtra("file_path"));
            arrayListName.add(intent.getStringExtra("file_name"));
            arrayListContainer.add(intent.getStringExtra("file_container"));
            arrayListVideo.add(itemVideoDownload);

            Message msg = mHandler.obtainMessage();
            msg.what = 0;
            mHandler.sendMessage(msg);
        }
    }

    private void handleCancelAction(@NonNull Intent intent) {
        String streamId = intent.getStringExtra("stream_id");
        if (streamId == null || streamId.isEmpty()) {
            return;
        }
        cancelDownloadByStreamId(streamId);
    }

    private boolean isVideoAlreadyAdded(ItemVideoDownload itemVideoDownload) {
        for (ItemVideoDownload video : arrayListVideo) {
            if (video.getStreamID().equals(itemVideoDownload.getStreamID())) {
                return true;
            }
        }
        return false;
    }

    private synchronized void cancelDownloadByStreamId(@NonNull String streamId) {
        int index = findDownloadIndex(streamId);
        if (index == -1) {
            return;
        }
        if (count > 0) {
            count--;
        }

        if (index == 0) {
            cancelActiveDownload();
        } else {
            deletePendingFile(index);
            removeQueueEntry(index);
        }

        if (arrayListURL.isEmpty()) {
            stop(null);
        } else {
            Message msg = mHandler.obtainMessage();
            msg.what = 0;
            mHandler.sendMessage(msg);
        }
    }

    private void cancelActiveDownload() {
        if (client != null) {
            for (Call runningCall : client.dispatcher().runningCalls()) {
                if (Objects.equals(runningCall.request().tag(), CANCEL_TAG)) {
                    runningCall.cancel();
                }
            }
        }
        if (call != null) {
            call.cancel();
            call = null;
        }
        if (thread != null) {
            thread.interrupt();
            thread = null;
        }
        deletePendingFile(0);
        removeQueueEntry(0);
        if (!arrayListURL.isEmpty()) {
            init();
        }
    }

    private int findDownloadIndex(@NonNull String streamId) {
        for (int i = 0; i < arrayListVideo.size(); i++) {
            ItemVideoDownload video = arrayListVideo.get(i);
            if (video != null && streamId.equals(video.getStreamID())) {
                return i;
            }
        }
        return -1;
    }

    private void removeQueueEntry(int index) {
        if (index >= 0 && index < arrayListVideo.size()) {
            arrayListVideo.remove(index);
        }
        if (index >= 0 && index < arrayListName.size()) {
            arrayListName.remove(index);
        }
        if (index >= 0 && index < arrayListContainer.size()) {
            arrayListContainer.remove(index);
        }
        if (index >= 0 && index < arrayListURL.size()) {
            arrayListURL.remove(index);
        }
        if (index >= 0 && index < arrayListFilePath.size()) {
            arrayListFilePath.remove(index);
        }
    }

    private void stop(Intent intent) {
        try {
            count = 0;
            if (client != null) {
                for (Call call1 : client.dispatcher().runningCalls()) {
                    if (Objects.equals(call1.request().tag(), CANCEL_TAG))
                        call1.cancel();
                }
            }
            if (thread != null) {
                thread.interrupt();
                thread = null;
            }
            deleteFirstFile(arrayListFilePath, arrayListName, arrayListContainer);

            arrayListVideo.clear();
            arrayListName.clear();
            arrayListContainer.clear();
            arrayListURL.clear();
            arrayListFilePath.clear();

            stopForeground();
            if (intent != null) {
                stopService(intent);
            } else {
                stopSelf();
            }
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "Error stopping service", e);
        }
    }

    public void deleteFirstFile(List<String> filePaths, List<String> names, List<String> containers) {
        deletePendingFileInternal(0, filePaths, names, containers);
    }

    private void deletePendingFile(int index) {
        deletePendingFileInternal(index, arrayListFilePath, arrayListName, arrayListContainer);
    }

    private void deletePendingFileInternal(int index,
                                           List<String> filePaths,
                                           List<String> names,
                                           List<String> containers) {
        if (filePaths == null || names == null || containers == null) {
            return;
        }
        if (index < 0
                || index >= filePaths.size()
                || index >= names.size()
                || index >= containers.size()) {
            return;
        }

        try {
            File baseDir = new File(filePaths.get(index));
            File file = new File(baseDir, names.get(index));

            boolean deleted = false;
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                deleted = Files.deleteIfExists(file.toPath());
            } else if (file.exists()) {
                deleted = file.delete();
            }

            if (deleted) {
                ApplicationUtil.log(TAG, "File deleted successfully");
            }
        } catch (IOException e) {
            Log.e(TAG, "Unexpected error during file deletion", e);
        }
    }

    public void init() {
        if (arrayListURL.isEmpty()) {
            return;
        }
        thread = new Thread(() -> {
            isDownloaded = false;

            client = new OkHttpClient.Builder()
                    .connectTimeout(30, java.util.concurrent.TimeUnit.SECONDS)
                    .readTimeout(3, java.util.concurrent.TimeUnit.MINUTES)
                    .writeTimeout(3, java.util.concurrent.TimeUnit.MINUTES)
                    .callTimeout(0, java.util.concurrent.TimeUnit.MILLISECONDS)
                    .retryOnConnectionFailure(true)
                    .build();
            DownloadContext context = createActiveContext();
            if (context == null) {
                return;
            }

            Request.Builder builder = new Request.Builder()
                    .url(context.url)
                    .addHeader("Accept-Encoding", "identity")
                    .get()
                    .tag(CANCEL_TAG);

            call = client.newCall(builder.build());
            call.enqueue(new Callback() {
                @Override
                public void onFailure(@NonNull Call call, @NonNull IOException e) {
                    handleFailure(context, e);
                }

                @Override
                public void onResponse(@NonNull Call call, @NonNull Response response) {
                    handleResponse(response, context);
                }
            });
        });
        thread.start();
    }

    private void handleResponse(Response response, @NonNull DownloadContext context) {
        if (response == null) {
            return;
        }

        if (!response.isSuccessful()) {
            Log.w(TAG, "Download failed with HTTP " + response.code() + " for " + context.streamId);
            closeQuietly(response);
            deleteActiveFile(context);
            finalizeContext(context);
            return;
        }

        ResponseBody rawBody = response.body();
        if (rawBody == null) {
            ApplicationUtil.log(TAG, "handleResponse: empty response body");
            closeQuietly(response);
            deleteActiveFile(context);
            finalizeContext(context);
            return;
        }

        ResponseBody progressBody = ProgressHelper.withProgress(rawBody, new ProgressUIListener() {
            @Override
            public void onUIProgressStart(long totalBytes) {
                super.onUIProgressStart(totalBytes);
                Message msg = mHandler.obtainMessage();
                msg.what = 0;
                mHandler.sendMessage(msg);
            }

            @Override
            public void onUIProgressChanged(long numBytes, long totalBytes, float percent, float speed) {
                if (Boolean.FALSE.equals(isDownloaded)) {
                    Message msg = mHandler.obtainMessage();
                    msg.what = 1;
                    msg.obj = (int) (100 * percent) + "";
                    mHandler.sendMessage(msg);
                }
            }
        });

        try {
            BufferedSource source = progressBody.source();
            enc.encrypt(context.filePath + "/" + context.fileName, source, context.video);
        } catch (Exception e) {
            ApplicationUtil.log(TAG, "Error encrypt", e);
            deleteActiveFile(context);
        } finally {
            closeQuietly(progressBody);
            closeQuietly(response);
        }

        finalizeContext(context);
    }

    private void handleFailure(@NonNull DownloadContext context, @NonNull IOException e) {
        if (call != null && call.isCanceled()) {
            Log.d(TAG, "Download cancelled: " + context.streamId);
        } else {
            Log.d(TAG, "Error in download: " + context.streamId, e);
        }
        deleteActiveFile(context);
        finalizeContext(context);
    }

    private void finalizeContext(@NonNull DownloadContext context) {
        if (!removeEntryByStreamId(context.streamId)) {
            return;
        }
        if (!arrayListURL.isEmpty()) {
            init();
        } else {
            notifyQueueFinished();
        }
    }

    private void notifyQueueFinished() {
        Message msg = mHandler.obtainMessage();
        msg.what = 2;
        msg.obj = "0";
        mHandler.sendMessage(msg);
        isDownloaded = true;
    }

    private boolean removeEntryByStreamId(@NonNull String streamId) {
        int index = findDownloadIndex(streamId);
        if (index == -1) {
            return false;
        }
        removeQueueEntry(index);
        return true;
    }

    private void closeQuietly(@Nullable Closeable closeable) {
        if (closeable == null) {
            return;
        }
        try {
            closeable.close();
        } catch (IOException ignored) {
            // Ignored
        }
    }

    @Nullable
    private synchronized DownloadContext createActiveContext() {
        if (arrayListVideo.isEmpty()
                || arrayListName.isEmpty()
                || arrayListFilePath.isEmpty()
                || arrayListURL.isEmpty()) {
            return null;
        }
        ItemVideoDownload video = arrayListVideo.get(0);
        if (video == null) {
            return null;
        }
        return new DownloadContext(
                video,
                arrayListURL.get(0),
                arrayListFilePath.get(0),
                arrayListName.get(0)
        );
    }

    private static class DownloadContext {
        private final ItemVideoDownload video;
        private final String url;
        private final String filePath;
        private final String fileName;
        private final String streamId;

        private DownloadContext(@NonNull ItemVideoDownload video,
                                @NonNull String url,
                                @NonNull String filePath,
                                @NonNull String fileName) {
            this.video = video;
            this.url = url;
            this.filePath = filePath;
            this.fileName = fileName;
            this.streamId = video.getStreamID();
        }
    }

    private void deleteActiveFile(@NonNull DownloadContext context) {
        File file = new File(context.filePath, context.fileName);
        try {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.O) {
                Files.deleteIfExists(file.toPath());
            } else if (file.exists()) {
                //noinspection ResultOfMethodCallIgnored
                file.delete();
            }
        } catch (IOException ioException) {
            ApplicationUtil.log(TAG, "Failed to delete partial file: " + file.getAbsolutePath(), ioException);
        }
    }
}
