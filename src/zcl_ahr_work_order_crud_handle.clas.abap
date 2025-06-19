CLASS zcl_ahr_work_order_crud_handle DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS:
      create_work_order IMPORTING io_out        TYPE REF TO if_oo_adt_classrun_out
                                  is_work_order TYPE ztahr_work_ord,
      read_work_order   IMPORTING io_out        TYPE REF TO if_oo_adt_classrun_out
                                  is_work_order TYPE ztahr_work_ord,
      update_work_order IMPORTING io_out        TYPE REF TO if_oo_adt_classrun_out
                                  is_work_order TYPE ztahr_work_ord,
      delete_work_order IMPORTING io_out        TYPE REF TO if_oo_adt_classrun_out
                                  is_work_order TYPE ztahr_work_ord.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_ahr_work_order_crud_handle IMPLEMENTATION.


  METHOD create_work_order.

    DATA(lo_work_order) = NEW zcl_ahr_work_order_validator( ).

    FINAL(lv_valid) = lo_work_order->validate_create_order( iv_customer_id   = CONV #( is_work_order-customer_id )
                                                            iv_technician_id = CONV #( is_work_order-technician_id )
                                                            iv_priority      = CONV #( is_work_order-priority ) ).

    IF lv_valid = abap_true.
      TRY.
          INSERT ztahr_work_ord FROM @is_work_order.
          io_out->write( |Se creo orden de trabajo en la base de datos| ).
          io_out->write( sy-dbcnt ).
          io_out->write( is_work_order ).
        CATCH cx_sy_open_sql_db.
          io_out->write( |No se pudo insertar registros de orden de trabajo| ).
      ENDTRY.
    ELSE.
      io_out->write( |Datos no validos para creación de orden de trabajo| ).
    ENDIF.

  ENDMETHOD.

  METHOD read_work_order.

    DATA(lo_work_order) = NEW zcl_ahr_work_order_validator( ).

*    FINAL(lv_valid) = lo_work_order->

  ENDMETHOD.

  METHOD update_work_order.

    DATA(lo_work_order) = NEW zcl_ahr_work_order_validator( ).

    FINAL(lv_valid) = lo_work_order->validate_update_order( iv_work_order_id = CONV #( is_work_order-work_order_id )
                                                            iv_status        = CONV #( is_work_order-status ) ).

    IF lv_valid = abap_true.

    ELSE.
      io_out->write( |Datos no validos para actualización de orden de trabajo| ).
    ENDIF.

  ENDMETHOD.

  METHOD delete_work_order.

    DATA(lo_work_order) = NEW zcl_ahr_work_order_validator( ).

    FINAL(lv_valid) = lo_work_order->validate_update_order( iv_work_order_id = '1'
                                                            iv_status        = 'PE' ).

    IF lv_valid = abap_true.

    ELSE.
*      io_out->write( |Se inserto registro en la base de datos| ).
    ENDIF.

  ENDMETHOD.

ENDCLASS.
