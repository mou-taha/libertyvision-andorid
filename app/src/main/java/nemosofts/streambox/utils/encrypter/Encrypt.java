package nemosofts.streambox.utils.encrypter;

import android.annotation.SuppressLint;
import android.content.Context;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;

import androidx.annotation.NonNull;

import java.io.BufferedOutputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.URL;

import javax.crypto.Cipher;
import javax.crypto.CipherOutputStream;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import javax.net.ssl.HttpsURLConnection;

import nemosofts.streambox.BuildConfig;
import nemosofts.streambox.item.ItemVideoDownload;
import nemosofts.streambox.utils.ApplicationUtil;
import nemosofts.streambox.utils.helper.DBHelper;
import okio.BufferedSource;

public class Encrypt {

    private static final String TRANSFORMATION = "AES/CTR/NoPadding";
    private static final String ALGORITHM = "AES";
    private static final int BUFFER_SIZE = 2048;

    private DBHelper dbHelper;
    private Context context;

    @SuppressLint("StaticFieldLeak")
    private static Encrypt instance = null;

    private Encrypt() {}

    public static Encrypt getInstance() {
        if (instance == null) {
            instance = new Encrypt();
        }
        return instance;
    }

    public void init(Context context) {
        this.context = context.getApplicationContext();
        dbHelper = new DBHelper(context);
    }

    public String getEditedFileName(File file, String token) {
        String path = file.getAbsolutePath();
        int i = path.lastIndexOf('.');
        return (i > 0 ? path.substring(0, i) : path) + token;
    }

    public void encrypt(String fileName, BufferedSource bufferedSource, final ItemVideoDownload itemDownload) {
        try {
            File fileEncrypt = new File(getEditedFileName(
                    new File(fileName.concat(ApplicationUtil.containerExtension(itemDownload.getContainerExtension()))), "")
            );
            final String fileSavedName = fileEncrypt.getName().replace(
                    ApplicationUtil.containerExtension(itemDownload.getContainerExtension()), ""
            );
            itemDownload.setTempName(fileSavedName);

            Cipher encryptionCipher = Cipher.getInstance(TRANSFORMATION);
            SecretKeySpec secretKeySpec = new SecretKeySpec(getEncryptionKey(), ALGORITHM);
            encryptionCipher.init(Cipher.ENCRYPT_MODE, secretKeySpec, getIvParameterSpec());

            try (InputStream fis = bufferedSource.inputStream();
                 OutputStream fileStream = new BufferedOutputStream(new FileOutputStream(fileEncrypt));
                 OutputStream outputStream = new CipherOutputStream(fileStream, encryptionCipher)) {

                byte[] buffer = new byte[BUFFER_SIZE];
                int len;
                while ((len = fis.read(buffer)) != -1) {
                    outputStream.write(buffer, 0, len);
                }
            }

            String imageName = downloadAndSaveThumbnail(itemDownload.getStreamIcon(), fileSavedName);
            itemDownload.setStreamIcon(imageName);
            itemDownload.setTempName(fileSavedName);
            String tableName = itemDownload.getDownloadTable();
            if (tableName == null || tableName.isEmpty()) {
                tableName = DBHelper.TABLE_DOWNLOAD_MOVIES;
            }
            dbHelper.addToDownloads(tableName, itemDownload);

        } catch (Exception e) {
            ApplicationUtil.log("Encrypt", "Error encrypting file",e);
        }
    }

    @NonNull
    private IvParameterSpec getIvParameterSpec() {
        byte[] initialIv = BuildConfig.IV.getBytes();
        return new IvParameterSpec(initialIv);
    }

    private byte[] getEncryptionKey() {
        String inputString = BuildConfig.ENC_KEY;
        return inputString.getBytes();
    }

    private String downloadAndSaveThumbnail(String imageUrl, String fileName) {
        if (imageUrl == null || imageUrl.isEmpty()) {
            return "null";
        }

        try (InputStream input = getInputStream(imageUrl)) {
            BitmapFactory.Options options = new BitmapFactory.Options();
            options.inPreferredConfig = Bitmap.Config.RGB_565; // Save memory
            Bitmap bitmap = BitmapFactory.decodeStream(input, null, options);
            if (bitmap == null) {
                return "null";
            }

            ByteArrayOutputStream bytes = new ByteArrayOutputStream();
            if (!bitmap.compress(Bitmap.CompressFormat.JPEG, 80, bytes)) {
                return "null";
            }

            File tempDir = new File(context.getExternalFilesDir(""), "tempim");
            if (!tempDir.exists() && !tempDir.mkdirs()) {
                return "null";
            }

            File outputFile = new File(tempDir, fileName + ".jpg");
            try (FileOutputStream fos = new FileOutputStream(outputFile)) {
                fos.write(bytes.toByteArray());
                return outputFile.getAbsolutePath();
            }
        } catch (IOException e) {
            return "null";
        }
    }

    private static InputStream getInputStream(String src) throws IOException {
        if (src == null || src.isEmpty()) {
            throw new IOException("Invalid URL: " + src);
        }

        URL url = new URL(src);
        HttpURLConnection connection = src.startsWith("https://")
                ? (HttpsURLConnection) url.openConnection()
                : (HttpURLConnection) url.openConnection();

        connection.setConnectTimeout(15000);
        connection.setReadTimeout(15000);
        connection.setDoInput(true);
        connection.connect();
        return connection.getInputStream();
    }
}
