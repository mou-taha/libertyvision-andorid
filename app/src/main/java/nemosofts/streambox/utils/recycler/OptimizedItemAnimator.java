package nemosofts.streambox.utils.recycler;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.DefaultItemAnimator;
import androidx.recyclerview.widget.RecyclerView;

public class OptimizedItemAnimator {

    private OptimizedItemAnimator() {
        throw new IllegalStateException("Utility class");
    }

    /**
     * Creates and returns an optimized ItemAnimator for RecyclerView with:
     * - ViewHolder reuse enabled for better performance
     * - All animations disabled for bulk operations
     *
     * @return Configured DefaultItemAnimator instance with performance optimizations
     */
    @NonNull
    public static RecyclerView.ItemAnimator create() {
        // Create an anonymous subclass of DefaultItemAnimator with optimized settings
        DefaultItemAnimator animator = new DefaultItemAnimator() {
            @Override
            public boolean canReuseUpdatedViewHolder(@NonNull RecyclerView.ViewHolder viewHolder) {
                // Allows view holder reuse for better performance with large datasets
                return true;
            }
        };
        // Disable all animations for better performance with bulk operations
        animator.setAddDuration(0);
        animator.setChangeDuration(0);
        animator.setMoveDuration(0);
        animator.setRemoveDuration(0);
        return animator;
    }
}