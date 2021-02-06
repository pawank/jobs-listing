package controllers

import shared.SelectObject

object HtmlUtils {
  val statusActionsForAds: Seq[SelectObject] = Seq(SelectObject("Pending","Pending", "grey"), SelectObject("Ignored","Ignored", "red"), SelectObject("Processed", "Processed", "blue"), SelectObject("Validated","Validated","purple"), SelectObject("Live","Live","green"))
}
