#' Given an angle between 0 and 360 degrees, return a color
#'
#' @param angle angle from 0 to 360 degrees
#'
#' @return hex color
#' @export
#'
#' @examples
#' angle_color(221)
angle_color<-function(angle){
  a<-subset(angle,angle>0 & angle<90)
  a<-length(a)
  colfunc1<-colorRampPalette(c("red","yellow"))
  c1<-colfunc1(a)
  b<-subset(angle,angle>=90 & angle<180)
  b<-length(b)
  colfunc2<-colorRampPalette(c("cyan","green"))
  c2<-colfunc2(b)
  c<-subset(angle,angle>=180 & angle<270)
  c<-length(c)
  colfunc3<-colorRampPalette(c("springgreen","blue"))
  c3<-colfunc3(c)
  d<-subset(angle,angle>=270 & angle<360)
  d<-length(d)
  colfunc4<-colorRampPalette(c("purple","pink"))
  c4<-colfunc4(d)
  color<-c(c1,c2,c3,c4)
  return(color)
}
