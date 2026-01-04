package nemosofts.streambox.item;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

public class ItemPostHome implements Serializable{

	String catID;
	String title;
	String type;
	ArrayList<ItemMovies> arrayListMovies = new ArrayList<>();
	ArrayList<ItemSeries> arrayListSeries = new ArrayList<>();
	ArrayList<ItemChannel> arrayListLive = new ArrayList<>();

	public ItemPostHome(String catID, String title, String type) {
		this.catID = catID;
		this.title = title;
		this.type = type;
	}

	public String getCatID() {
		return catID;
	}
	public String getTitle() {
		return title;
	}
	public String getType() {
		return type;
	}

	public List<ItemMovies> getArrayListMovies() {
		return arrayListMovies;
	}
	public void setArrayListMovies(List<ItemMovies> arrayList) {
        if (!this.arrayListMovies.isEmpty()){
            this.arrayListMovies.clear();
        }
		this.arrayListMovies.addAll(arrayList);
	}

	public List<ItemSeries> getArrayListSeries() {
		return arrayListSeries;
	}
	public void setArrayListSeries(List<ItemSeries> arrayList) {
        if (!this.arrayListSeries.isEmpty()){
            this.arrayListSeries.clear();
        }
		this.arrayListSeries.addAll(arrayList);
	}

	public List<ItemChannel> getArrayListLive() {
		return arrayListLive;
	}
	public void setArrayListLive(List<ItemChannel> arrayList) {
        if (!this.arrayListLive.isEmpty()){
            this.arrayListLive.clear();
        }
		this.arrayListLive.addAll(arrayList);
	}
}