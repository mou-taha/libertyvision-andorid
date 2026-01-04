package nemosofts.streambox.item;

import java.io.Serializable;

public class ItemSelectPage implements Serializable {

	private final String id;
	private final String title;
	private final String type;
	private final String baseURL;

	/**
	 * Constructs an ItemSelectPage object.
	 *
	 * @param id Page ID
	 * @param title Page name
	 * @param type Page type
	 * @param base URL
	 */

	public ItemSelectPage(String id, String title, String type, String base) {
		this.id = id;
		this.title = title;
		this.type = type;
		this.baseURL = base;
	}

	public String getId() {
		return id;
	}

	public String getTitle() {
		return title;
	}

	public String getType() {
		return type;
	}

	public String getBase() {
		return baseURL;
	}
}