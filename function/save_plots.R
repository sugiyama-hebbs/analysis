# This saves the main plots in pdf, eps, and svg.
# By default, it saves the last plot
# Also, make sure that directory and output names are correctly defined if you don't pass the inputs manually. 
#
# Author: Taisei Sugiyama

library(Cairo)

save_plots = function(tgt_plot = last_plot(), mdir = main_dir, sdir = sub_dir, fname = fig_name, readme_content = desc_note, pdf_only = F){
  # Set path
  mdir_path = sprintf("figure/%s",mdir)
  # Create a folder if not exist
  dir.create(file.path("figure", mdir), showWarnings = FALSE) 
  dir.create(file.path(mdir_path, sdir), showWarnings = FALSE)
  dir.create(file.path("figure",mdir), showWarnings = FALSE)
  dir.create(file.path(sprintf("%s/%s",mdir_path,sdir), "eps"), showWarnings = FALSE)
  dir.create(file.path(sprintf("%s/%s",mdir_path,sdir), "svg"), showWarnings = FALSE)
  dir.create(file.path(sprintf("%s/%s",mdir_path,sdir), "nolegend"), showWarnings = FALSE)
  
  save_dir = sprintf("figure/%s/%s",mdir,sdir)
  
  fname_plot_pdf = sprintf("%s/%s.pdf",save_dir,fname)
  fname_plot_pdf_noleg = sprintf("%s/nolegend/%s.pdf",save_dir,fname)
  fname_plot_eps = sprintf("%s/eps/%s.eps",save_dir,fname)
  fname_plot_svg = sprintf("%s/svg/%s.svg",save_dir,fname)
  fname_readme = sprintf("%s/%s.txt",save_dir,sdir)
  
  
  ## Now output the plots ##
  # pdf
  ggsave(fname_plot_pdf, plot = tgt_plot, width = 20, height = 20, units = "cm")
  plot.noleg = tgt_plot +
    theme(legend.position = "none") # plot withour legend
  
  ggsave(fname_plot_pdf_noleg, plot = plot.noleg, width = 20, height = 20, units = "cm", useDingbats=FALSE)
  
  # Others
  if (!pdf_only){
    ggsave(fname_plot_eps, plot = tgt_plot, width = 20, height = 20, units = "cm", device = cairo_ps)
    ggsave(fname_plot_svg, plot = tgt_plot, width = 20, height = 20, units = "cm")
    # Spit out readme
    readme = file(fname_readme)
    writeLines(readme_content,
               readme)
    close(readme)
  }
  
  
  
  
}