#Forest loss study
#Add all attributes to loss_stat_frames
#08/06/2025

#Load in frame with all attributes
attr = read.csv('D:/Forest_loss/Complete_attr_stats_frames/1yr.csv')

#Load in frame with EI & RB calculated with loss year
loss_stat = read.csv('D:/Forest_loss/Complete_attr_stats_frames/loss_stat_frames/all_1yr.csv')

merged = merge(loss_stat, attr[, c(5, 20:223)], by = "gauge_name")

write.csv(merged, 'D:/Forest_loss/Complete_attr_stats_frames/1yr.csv')
