module parameter
    implicit none
    type par
        real:: kc, um, lm, c,    & !蒸发计算参数
             & wm, b, im,        & !产流计算参数
             & fc,               & !二水源划分参数
             & sm, ex, kg, ki,   & !三水源划分参数
             & uh,               & !单位线坡面汇流参数
             & ci, cg,           & !线性水库坡面汇流参数
             & cs, l,            & !滞后算法坡面汇、河道流参数
             & ke, xe              !马斯京根河道汇流参数
    end type
contains
    
end module parameter