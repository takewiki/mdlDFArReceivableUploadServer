#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' arReceivableUploadServer()
arReceivableUploadServer <- function(input,output,session,dms_token) {

  options(shiny.maxRequestSize = 30 * 1024^2)
  #获取参数
  text_arReceivable_upload = tsui::var_file('text_arReceivable_upload')

  shiny::observeEvent(input$btn_arReceivable_upload,{

    filename=text_arReceivable_upload()

    if(filename==''  || is.null(filename)){

      tsui::pop_notice("请先上传文件")


    }else{

      # 清空临时表

      mdlDFArReceivableUploadPkg::arReceivable_delete(dms_token = dms_token)


      data <- readxl::read_excel(filename,col_types = c("text", "numeric", "text",
                                                        "text", "text", "text", "text", "text",
                                                        "text", "text", "text", "numeric",
                                                        "text", "text", "text", "numeric",
                                                        "text", "text", "text", "text", "text",
                                                        "text", "text", "text", "text", "text"))



      data = as.data.frame(data)
      data = tsdo::na_standard(data)

      tsda::mysql_writeTable2(token = dms_token,table_name = 'rds_erp_byd_src_t_ar_receivable_list_input',r_object = data,append = TRUE)


      # 插入list表和表头表体

      mdlDFArReceivableUploadPkg::arReceivable_insert(dms_token = dms_token)

      tsui::pop_notice("上传成功")


    }


  })



}



#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' arReceivableViewServer()
arReceivableViewServer <- function(input,output,session,dms_token) {

  #获取参数
  text_arReceivable_daterange = tsui::var_dateRange('text_arReceivable_daterange')

  shiny::observeEvent(input$btn_arReceivable_view,{

    FDate = text_arReceivable_daterange()

    FStartDate = FDate[1]

    FEndDate = FDate[2]

    data = mdlDFArReceivableUploadPkg::arReceivable_select(dms_token = dms_token,FStartDate =FStartDate ,FEndDate = FEndDate)

    tsui::run_dataTable2(id = 'arReceivable_resultView',data = data)

    tsui::run_download_xlsx(id = 'dl_arReceivable',data = data,filename = 'BYD应收单.xlsx')




  })



}


#' 处理逻辑
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples
#' arReceivableServer()
arReceivableServer <- function(input,output,session,dms_token) {

  arReceivableUploadServer(input = input,output = output,session = session,dms_token = dms_token)



  arReceivableViewServer(input = input,output = output,session = session,dms_token = dms_token)


}
