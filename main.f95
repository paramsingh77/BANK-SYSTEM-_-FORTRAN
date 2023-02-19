program banking

  character(len=30)::first_name , last_name
  real::current_balance
  integer::option,selection,loop_count

  call account_setup(first_name,last_name,current_balance)
  
  Write(*,*) "Enter 1 for CONTINUE , 0 for EXIT"
  Read(*,*) loop_count
  Write(*,*)
  
  do while(loop_count .ne. 0)
        call print_menu()
        option = getting_user_selection(selection)

        if(option == 1)then
            call withdrawal_process(current_balance)
        else if(option == 2)then
            call process_deposit(current_balance)
        else if(option == 3)then
            call print_balance(current_balance)
        end if

        Write(*,*) "Enter 1 for CONTINUE , 0 for EXIT"
        Read(*,*) loop_count
        WRITE(*,*)
  end do
   call print_receipt(first_name,last_name,current_balance)

  
  contains
     subroutine account_setup(first_name,last_name,account_balance)

            character(len=30),intent(out)::first_name,last_name
            real,intent(out)::account_balance
             Write(*,*)"************ACCOUNT SETUP********************"
             Write(*,*) "Enter Name: "
             Read(*,*) first_name,last_name
             Write(*,*) "Enter Account Balance: "
             Read(*,*) account_balance
             Write(*,*)"**********************************************"
     
     end subroutine account_setup


     subroutine print_menu()
     
         Write(*,*)"***WELCOME TO WEALTHY PEOPLE ONLY BANK***"
         Write(*,*)"1 - Make Withdrawal"
         Write(*,*)"2 - Make Deposit"
         Write(*,*)"3 - Get Current Account Balance"
         Write(*,*)"0 - Logout"
         WRITE(*,*)"****************************************"
         Write(*,*)
     end subroutine print_menu


     integer function getting_user_selection(selection)
              integer::selection
              logical:: status

              status = .false.
              do while( .not. status)
                  Write(*,*) "Enter option you want to choose: "
                  Read(*,*) selection
                  status  = validate(selection)
              end do
            getting_user_selection = selection
            
     end function getting_user_selection


     logical function validate(selection)
              integer:: selection
              logical::status
              status = .true.
              if(selection <0 .or. selection >3) then
                    status = .false.
              end if
            validate = status
     end function validate


     subroutine print_balance(current_balance)
          real,intent(in)::current_balance
          Write(*,*)  "Current Balance: ",current_balance
     end subroutine print_balance


    logical function check_over_draft(current_balance,desired_balance)
           real::current_balance,desired_balance
           logical::status

           status = .false.

           if(current_balance < desired_balance)then
                status = .true.
           end if
        check_over_draft = status
  
    end function check_over_draft


    subroutine withdrawal_process(current_balance)
           real,intent(inout)::current_balance
           real::withdraw_amount
           logical::answer

           Write(*,*) "Enter amount of money to be withdrawn: "
           Read(*,*) withdraw_amount
           
           answer = check_over_draft(current_balance,withdraw_amount)

           if(answer .eqv. .false.)then
              current_balance = current_balance - withdraw_amount
           else 
              Write(*,*) "WITHDRAWAL DENIED...."
          end if
             
    end subroutine withdrawal_process

    subroutine process_deposit(current_balance)
          real,intent(inout)::current_balance
          real::depost_amount

          Write(*,*) "Enter the amount to be deposited: "
          Read(*,*) deposit_amount

          if(deposit_amount >= 0 )then
              current_balance = current_balance + deposit_amount
          else 
              Write(*,*) "DEPOSIT DENIED....."
          end if
          
    end subroutine process_deposit

    subroutine print_receipt(first_name,last_name,current_balance)
        character(len=30),intent(in)::first_name,last_name
        real,intent(in)::current_balance

        Write(*,*) "**********RECEIPT************"
        WRITE(*,*) "NAME:    ",first_name,last_name
        Write(*,*) "BALANCE: ",current_balance
        Write(*,*) "*****************************"
        
    end subroutine print_receipt
    
    
    
end program banking