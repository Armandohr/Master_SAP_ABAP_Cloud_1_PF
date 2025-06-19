CLASS zcl_ahr_work_order_crud_test DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .

    METHODS:
      test_create_work_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
      test_read_work_order   IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
      test_update_work_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out,
      test_delete_work_order IMPORTING io_out TYPE REF TO if_oo_adt_classrun_out.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS:
      m_init_catalog_technician,
      m_init_catalog_customer.

ENDCLASS.

CLASS zcl_ahr_work_order_crud_test IMPLEMENTATION.

  METHOD if_oo_adt_classrun~main.

    m_init_catalog_customer( ).

    m_init_catalog_technician( ).

    test_create_work_order( io_out = out ).
*
*    test_read_work_order( io_out = out ).
*
    test_update_work_order( io_out = out ).
*
*    test_delete_work_order( io_out = out ).

  ENDMETHOD.

  METHOD m_init_catalog_customer.

    DELETE FROM ztahr_customer.

    INSERT ztahr_customer FROM TABLE @( VALUE #( ( customer_id = |{ '1' ALPHA = IN }|
                                                   name        = 'Howard Wolowitz'
                                                   address     = 'Pasadena #2345, California'
                                                   phone       = '5512345678' )
                                                 ( customer_id = |{ '2' ALPHA = IN }|
                                                   name        = 'Raj Koothrappali'
                                                   address     = 'Pasadena #1234, California'
                                                   phone       = '5523456789' )
                                               ) ).

  ENDMETHOD.

  METHOD m_init_catalog_technician.

    DELETE FROM ztahr_technician.

    INSERT ztahr_technician FROM TABLE @( VALUE #( ( technician_id = |{ '1' ALPHA = IN }|
                                                     name          = 'Leonard Hofstadter'
                                                     specialty     = 'Actor 1' )
                                                   ( technician_id = |{ '2' ALPHA = IN }|
                                                      name         = 'Sheldon Cooper'
                                                     specialty     = 'Actor 2' )
                                                 ) ).
  ENDMETHOD.

  METHOD test_create_work_order.

    DATA(lo_work_order) = NEW zcl_ahr_work_order_crud_handle( ).

    lo_work_order->create_work_order( io_out = io_out
                                      is_work_order = VALUE #( work_order_id = |{ '1' ALPHA = IN }|
                                                               customer_id   = |{ '1' ALPHA = IN }|
                                                               technician_id = |{ '1' ALPHA = IN }|
                                                               creation_date = cl_abap_context_info=>get_system_date( )
                                                               status        = 'PE'
                                                               priority      = 'A'
                                                               description   = 'Primera orden creada'
                                                               client        = '100'
                                                             ) ).

    lo_work_order->create_work_order( io_out = io_out
                                      is_work_order = VALUE #( work_order_id = |{ '2' ALPHA = IN }|
                                                               customer_id   = |{ '1' ALPHA = IN }|
                                                               technician_id = |{ '1' ALPHA = IN }|
                                                               creation_date = cl_abap_context_info=>get_system_date( )
                                                               status        = 'CO'
                                                               priority      = 'A'
                                                               description   = 'Orden creada - completada'
                                                               client        = '100'
                                                             ) ).

  ENDMETHOD.

  METHOD test_read_work_order.

  ENDMETHOD.

  METHOD test_update_work_order.

    DATA(lo_work_order) = NEW zcl_ahr_work_order_crud_handle( ).

    lo_work_order->update_work_order( io_out        = io_out
                                      is_work_order = VALUE #( work_order_id = |{ '1' ALPHA = IN }|
                                                               customer_id   = |{ '1' ALPHA = IN }|
                                                               technician_id = |{ '1' ALPHA = IN }|
                                                               creation_date = cl_abap_context_info=>get_system_date( )
                                                               status        = 'PE'
                                                               priority      = 'A'
                                                               description   = 'Primera actualización de orden de trabajo'
                                                               client        = '100' ) ).

    lo_work_order->update_work_order( io_out        = io_out
                                      is_work_order = VALUE #( work_order_id = |{ '2' ALPHA = IN }|
                                                               customer_id   = |{ '1' ALPHA = IN }|
                                                               technician_id = |{ '1' ALPHA = IN }|
                                                               creation_date = cl_abap_context_info=>get_system_date( )
                                                               status        = 'CO'
                                                               priority      = 'A'
                                                               description   = 'Segunda actualización de orden de trabajo'
                                                               client        = '100' ) ).

  ENDMETHOD.

  METHOD test_delete_work_order.

  ENDMETHOD.

ENDCLASS.
