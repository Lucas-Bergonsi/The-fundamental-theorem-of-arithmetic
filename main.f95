  Program primos
         
       Integer:: numero,p,numerofatores
       Integer, dimension(:), allocatable :: fatores
       Print*,'==============================================='
       Print*, 'Entre com o n£mero que quer decompor:'
       Read*, numero
       allocate(fatores(numero))

        p=2 !Valor do m¢dulo
        numerofatores=1 !inicia a contagem de fatores
        do
        if(mod(numero,p)==0) then !O n£mero ‚ divisivel por 2,resto=0
               fatores(numerofatores)=p
                numerofatores= numerofatores+1
               numero=numero/p
        else!Caso contr rio, passa para o pr¢ximo divisor
                p=p+1
             end if
        if(numero==1) then
                numerofatores=numerofatores-1
                 exit
            end if
        end do
           Print*,'==============================================='
           Print*,' A decomposi‡Æo ‚:'
           Print*,numero, " = 1 ",(" x ",fatores(p),p=1,numerofatores)
           Print*,'==============================================='

        pause
        
       end program primos
