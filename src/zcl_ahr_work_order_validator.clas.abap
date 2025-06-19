CLASS zcl_ahr_work_order_validator DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS validate_create_order IMPORTING iv_customer_id   TYPE string
                                            iv_technician_id TYPE string
                                            iv_priority      TYPE string
                                  RETURNING VALUE(rv_valid)  TYPE abap_bool.

    METHODS validate_update_order IMPORTING iv_work_order_id TYPE string
                                            iv_status        TYPE string
                                  RETURNING VALUE(rv_valid)  TYPE abap_bool.

    METHODS validate_delete_order IMPORTING iv_work_order_id TYPE string
                                            iv_status        TYPE string
                                  RETURNING VALUE(rv_valid)  TYPE abap_bool.

    METHODS validate_status_and_priority IMPORTING iv_status       TYPE string
                                                   iv_priority     TYPE string
                                         RETURNING VALUE(rv_valid) TYPE abap_bool.

    METHODS validate_read_order IMPORTING iv_work_order_id TYPE string
                                RETURNING VALUE(rv_valid)  TYPE abap_bool.

  PROTECTED SECTION.

  PRIVATE SECTION.

    CONSTANTS: c_valid_status   TYPE string VALUE 'PE CO', " Example statuses: Pending, Completed
               c_valid_priority TYPE string VALUE 'A B'. " Example priorities: High, Low

    " Helper methods to check the existence of customer, technician, order, etc.
    METHODS check_customer_exists IMPORTING iv_customer_id   TYPE string
                                  RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_technician_exists IMPORTING iv_technician_id TYPE string
                                    RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_order_exists IMPORTING iv_work_order_id TYPE string
                               RETURNING VALUE(rv_exists) TYPE abap_bool.

    METHODS check_order_history IMPORTING iv_work_order_id TYPE string
                                RETURNING VALUE(rv_exists) TYPE abap_bool.

ENDCLASS.


CLASS zcl_ahr_work_order_validator IMPLEMENTATION.

  METHOD validate_create_order.

    " Check if customer exists
    FINAL(lv_customer_exists) = check_customer_exists( iv_customer_id ).
    IF lv_customer_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if technician exists
    FINAL(lv_technician_exists) = check_technician_exists( iv_technician_id ).
    IF lv_technician_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if priority is valid
*     IF iv_priority NOT IN c_valid_priority.
    IF iv_priority NA c_valid_priority.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD validate_update_order.

    " Check if the work order exists
    FINAL(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order status is editable (e.g., Pending)
* IF iv_status NOT IN c_valid_status.
    IF iv_status NA c_valid_status.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD validate_delete_order.

    " Check if the order exists
    FINAL(lv_order_exists) = check_order_exists( iv_work_order_id ).
    IF lv_order_exists IS INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order status is "PE" (Pending)
    IF iv_status <> 'PE'.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Check if the order has a history (i.e., if it has been modified before)
    FINAL(lv_has_history) = check_order_history( iv_work_order_id ).
    IF lv_has_history IS NOT INITIAL.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD validate_status_and_priority.

    " Validate the status value
* IF iv_status NOT IN c_valid_status.
    IF iv_status NA c_valid_status.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    " Validate the priority value
* IF iv_priority NOT IN c_valid_priority.
    IF iv_priority NA c_valid_priority.
      rv_valid = abap_false.
      RETURN.
    ENDIF.

    rv_valid = abap_true.

  ENDMETHOD.

  METHOD check_customer_exists.

    SELECT SINGLE FROM ztahr_customer
    FIELDS customer_id
     WHERE customer_id = @iv_customer_id
      INTO @DATA(lv_customer).
    IF sy-subrc = 0.
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD check_order_exists.

    SELECT SINGLE FROM ztahr_work_ord
    FIELDS work_order_id
     WHERE work_order_id = @iv_work_order_id
      INTO @DATA(lv_work_order).
    IF sy-subrc = 0.
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD check_order_history.

    SELECT SINGLE FROM ztahr_work_ord_h
    FIELDS work_order_id
     WHERE work_order_id = @iv_work_order_id
      INTO @DATA(lv_work_order_h).
    IF sy-subrc = 0.
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD check_technician_exists.

    SELECT SINGLE FROM ztahr_technician
    FIELDS technician_id
     WHERE technician_id = @iv_technician_id
      INTO @DATA(lv_technician).
    IF sy-subrc = 0.
      rv_exists = abap_true.
    ELSE.
      rv_exists = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD validate_read_order.

  ENDMETHOD.

ENDCLASS.
