/*
   Copyright Manuel Freiberger 2013.
   Distributed under the Boost Software License, Version 1.0.
   (See accompanying file LICENSE_1_0.txt or copy at
   http://www.boost.org/LICENSE_1_0.txt)
*/

#ifndef PERIDEF_STM32F4XX_H
#define PERIDEF_STM32F4XX_H

#include <stdint.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct
{
    volatile uint32_t TIR;
    volatile uint32_t TDTR;
    volatile uint32_t TDLR;
    volatile uint32_t TDHR;
} CAN_TxMailBox_TypeDef;

typedef struct
{
    volatile uint32_t RIR;
    volatile uint32_t RDTR;
    volatile uint32_t RDLR;
    volatile uint32_t RDHR;
} CAN_FifoMailBox_TypeDef;

typedef struct
{
    volatile uint32_t FR1;
    volatile uint32_t FR2;
} CAN_FilterRegister_TypeDef;

typedef struct
{
    volatile uint32_t MCR;
    volatile uint16_t MSR;
    uint16_t reserved0;
    volatile uint32_t TSR;
    volatile uint32_t RF0R;
    volatile uint32_t RF1R;
    volatile uint32_t IER;
    volatile uint32_t ESR;
    volatile uint32_t BTR;
    uint32_t reserved1[88];
    CAN_TxMailBox_TypeDef TxMailBox[3];
    CAN_FifoMailBox_TypeDef FifoMailBox[2];
    uint32_t reserved2[12];
    volatile uint16_t FMR;
    uint16_t reserved3;
    volatile uint32_t FM1R;
    uint32_t reserved4;
    volatile uint32_t FS1R;
    uint32_t reserved5;
    volatile uint32_t FFA1R;
    uint32_t reserved6;
    volatile uint32_t FA1R;
    uint32_t reserved7[8];
    CAN_FilterRegister_TypeDef FilterRegister[28];
} CAN_TypeDef;

typedef struct
{
    volatile uint32_t MODER;
    volatile uint16_t OTYPER;
    uint16_t reserved0;
    volatile uint32_t OSPEED;
    volatile uint32_t PUPDR;
    volatile uint16_t IDR;
    uint16_t reserved1;
    volatile uint16_t ODR;
    uint16_t reserved2;
    volatile uint16_t BSR;
    volatile uint16_t BRR;
    volatile uint32_t LCKR;
    volatile uint32_t AFR[2];
} GPIO_TypeDef;

typedef struct
{
    volatile uint32_t CR;
    volatile uint32_t PLLCFGR;
    volatile uint32_t CFGR;
    volatile uint32_t CIR;
    volatile uint32_t AHB1RSTR;
    volatile uint32_t AHB2RSTR;
    volatile uint32_t AHB3RSTR;
    uint32_t reserved0;
    volatile uint32_t APB1RSTR;
    volatile uint32_t APB2RSTR;
    uint32_t reserved1[2];
    volatile uint32_t AHB1ENR;
    volatile uint32_t AHB2ENR;
    volatile uint32_t AHB3ENR;
    uint32_t reserved2;
    volatile uint32_t APB1ENR;
    volatile uint32_t APB2ENR;
    uint32_t reserved3[2];
    volatile uint32_t AHB1LPENR;
    volatile uint32_t AHB2LPENR;
    volatile uint32_t AHB3LPENR;
    uint32_t reserved4;
    volatile uint32_t APB1LPENR;
    volatile uint32_t APB2LPENR;
    uint32_t reserved5[2];
    volatile uint32_t BDCR;
    volatile uint32_t CSR;
    uint32_t reserved6[2];
    volatile uint32_t SSCGR;
    volatile uint32_t PLLI2SCFGR;
} RCC_TypeDef;

typedef struct
{
    volatile uint16_t SR;
    uint16_t reserved0;
    volatile uint16_t DR;
    uint16_t reserved1;
    volatile uint16_t BRR;
    uint16_t reserved2;
    volatile uint16_t CR1;
    uint16_t reserved3;
    volatile uint16_t CR2;
    uint16_t reserved4;
    volatile uint16_t CR3;
    uint16_t reserved5;
    volatile uint16_t GTPR;
    uint16_t reserved6;
} USART_TypeDef;


#define PERIPHERAL_BASE                ((uintptr_t)0x40000000)
#define APB1_BASE                      (PERIPHERAL_BASE + (uintptr_t)0x00000000)
#define APB2_BASE                      (PERIPHERAL_BASE + (uintptr_t)0x00010000)
#define AHB1_BASE                      (PERIPHERAL_BASE + (uintptr_t)0x00020000)
#define AHB2_BASE                      (PERIPHERAL_BASE + (uintptr_t)0x10000000)
#define AHB3_BASE                      (PERIPHERAL_BASE + (uintptr_t)0x60000000)

#define GPIOA                          ((GPIO_TypeDef*)(GPIO))
#define GPIOB                          ((GPIO_TypeDef*)(GPIO))
#define GPIOC                          ((GPIO_TypeDef*)(GPIO))
#define GPIOD                          ((GPIO_TypeDef*)(GPIO))
#define GPIOE                          ((GPIO_TypeDef*)(GPIO))
#define GPIOF                          ((GPIO_TypeDef*)(GPIO))
#define GPIOG                          ((GPIO_TypeDef*)(GPIO))
#define GPIOH                          ((GPIO_TypeDef*)(GPIO))
#define GPIOI                          ((GPIO_TypeDef*)(GPIO))
#define RCC                            ((RCC_TypeDef*)(RCC))
#define USART2                         ((USART_TypeDef*)(USART))
#define USART3                         ((USART_TypeDef*)(USART))
#define UART4                          ((USART_TypeDef*)(USART))
#define UART5                          ((USART_TypeDef*)(USART))
#define USART1                         ((USART_TypeDef*)(USART))
#define USART6                         ((USART_TypeDef*)(USART))

/* Bit fields of the CAN_TIR register */
#define CAN_TIR_TXRQ                   ((uint32_t)0x00000001)
#define CAN_TIR_RTR                    ((uint32_t)0x00000002)
#define CAN_TIR_IDE                    ((uint32_t)0x00000004)
#define CAN_TIR_EXID                   ((uint32_t)0x001FFFF8)
#define CAN_TIR_STID                   ((uint32_t)0xFFE00000)

/* Bit fields of the CAN_TDTR register */
#define CAN_TDTR_DLC                   ((uint32_t)0x0000000F)
#define CAN_TDTR_TGT                   ((uint32_t)0x00000100)
#define CAN_TDTR_TIME                  ((uint32_t)0xFFFF0000)

/* Bit fields of the CAN_RIR register */
#define CAN_RIR_RTR                    ((uint32_t)0x00000002)
#define CAN_RIR_IDE                    ((uint32_t)0x00000004)
#define CAN_RIR_EXID                   ((uint32_t)0x001FFFF8)
#define CAN_RIR_STID                   ((uint32_t)0xFFE00000)

/* Bit fields of the CAN_RDTR register */
#define CAN_RDTR_DLC                   ((uint32_t)0x0000000F)
#define CAN_RDTR_FMI                   ((uint32_t)0x0000FF00)
#define CAN_RDTR_TIME                  ((uint32_t)0xFFFF0000)

/* Bit fields of the RCC_CR register */
#define RCC_CR_HSION                   ((uint32_t)0x00000001)
#define RCC_CR_HSIRDY                  ((uint32_t)0x00000002)
#define RCC_CR_HSITRIM                 ((uint32_t)0x000000F8)
#define RCC_CR_HSICAL                  ((uint32_t)0x0000FF00)
#define RCC_CR_HSEON                   ((uint32_t)0x00010000)
#define RCC_CR_HSERDY                  ((uint32_t)0x00020000)
#define RCC_CR_HSEBYP                  ((uint32_t)0x00040000)
#define RCC_CR_CSSON                   ((uint32_t)0x00080000)
#define RCC_CR_PLLON                   ((uint32_t)0x01000000)
#define RCC_CR_PLLRDY                  ((uint32_t)0x02000000)
#define RCC_CR_PLLI2SON                ((uint32_t)0x04000000)
#define RCC_CR_PLLI2RDY                ((uint32_t)0x08000000)

/* Bit fields of the RCC_PLLCFGR register */
#define RCC_PLLCFGR_PLLM               ((uint32_t)0x0000003F)
#define RCC_PLLCFGR_PLLN               ((uint32_t)0x00007FC0)
#define RCC_PLLCFGR_PLLP               ((uint32_t)0x00030000)
#define RCC_PLLCFGR_PLLSRC             ((uint32_t)0x00400000)
#define RCC_PLLCFGR_PLLQ               ((uint32_t)0x0F000000)

/* Bit fields of the RCC_AHB1RSTR register */
#define RCC_AHB1RSTR_GPIOARST          ((uint32_t)0x00000001)
#define RCC_AHB1RSTR_GPIOBRST          ((uint32_t)0x00000002)
#define RCC_AHB1RSTR_GPIOCRST          ((uint32_t)0x00000004)
#define RCC_AHB1RSTR_GPIODRST          ((uint32_t)0x00000008)
#define RCC_AHB1RSTR_GPIOERST          ((uint32_t)0x00000010)
#define RCC_AHB1RSTR_GPIOFRST          ((uint32_t)0x00000020)
#define RCC_AHB1RSTR_GPIOGRST          ((uint32_t)0x00000040)
#define RCC_AHB1RSTR_GPIOHRST          ((uint32_t)0x00000080)
#define RCC_AHB1RSTR_GPIOIRST          ((uint32_t)0x00000100)
#define RCC_AHB1RSTR_CRCRST            ((uint32_t)0x00001000)
#define RCC_AHB1RSTR_DMA1RST           ((uint32_t)0x00200000)
#define RCC_AHB1RSTR_DMA2RST           ((uint32_t)0x00400000)
#define RCC_AHB1RSTR_ETHMACRST         ((uint32_t)0x02000000)
#define RCC_AHB1RSTR_OTGHSRST          ((uint32_t)0x20000000)

/* Bit fields of the RCC_AHB2RSTR register */
#define RCC_AHB2RSTR_DCMIRST           ((uint32_t)0x00000001)
#define RCC_AHB2RSTR_CRYPRST           ((uint32_t)0x00000010)
#define RCC_AHB2RSTR_HASHRST           ((uint32_t)0x00000020)
#define RCC_AHB2RSTR_RNGRST            ((uint32_t)0x00000040)
#define RCC_AHB2RSTR_OTGFSRST          ((uint32_t)0x00000080)

/* Bit fields of the RCC_AHB3RSTR register */
#define RCC_AHB3RSTR_FSMCRST           ((uint32_t)0x00000001)

/* Bit fields of the RCC_APB1RSTR register */
#define RCC_APB1RSTR_TIM2RST           ((uint32_t)0x00000001)
#define RCC_APB1RSTR_TIM3RST           ((uint32_t)0x00000002)
#define RCC_APB1RSTR_TIM4RST           ((uint32_t)0x00000004)
#define RCC_APB1RSTR_TIM5RST           ((uint32_t)0x00000008)
#define RCC_APB1RSTR_TIM6RST           ((uint32_t)0x00000010)
#define RCC_APB1RSTR_TIM7RST           ((uint32_t)0x00000020)
#define RCC_APB1RSTR_TIM12RST          ((uint32_t)0x00000040)
#define RCC_APB1RSTR_TIM13RST          ((uint32_t)0x00000080)
#define RCC_APB1RSTR_TIM14RST          ((uint32_t)0x00000100)
#define RCC_APB1RSTR_WWDGRST           ((uint32_t)0x00000800)
#define RCC_APB1RSTR_SPI2RST           ((uint32_t)0x00004000)
#define RCC_APB1RSTR_SPI3RST           ((uint32_t)0x00008000)
#define RCC_APB1RSTR_USART2RST         ((uint32_t)0x00020000)
#define RCC_APB1RSTR_USART3RST         ((uint32_t)0x00040000)
#define RCC_APB1RSTR_UART4RST          ((uint32_t)0x00080000)
#define RCC_APB1RSTR_UART5RST          ((uint32_t)0x00100000)
#define RCC_APB1RSTR_I2C1RST           ((uint32_t)0x00200000)
#define RCC_APB1RSTR_I2C2RST           ((uint32_t)0x00400000)
#define RCC_APB1RSTR_I2C3RST           ((uint32_t)0x00800000)
#define RCC_APB1RSTR_CAN1RST           ((uint32_t)0x02000000)
#define RCC_APB1RSTR_CAN2RST           ((uint32_t)0x04000000)
#define RCC_APB1RSTR_PWRRST            ((uint32_t)0x10000000)
#define RCC_APB1RSTR_DACRST            ((uint32_t)0x20000000)

/* Bit fields of the RCC_APB2RSTR register */
#define RCC_APB2RSTR_TIM1RST           ((uint32_t)0x00000001)
#define RCC_APB2RSTR_TIM8RST           ((uint32_t)0x00000002)
#define RCC_APB2RSTR_USART1RST         ((uint32_t)0x00000010)
#define RCC_APB2RSTR_USART6RST         ((uint32_t)0x00000020)
#define RCC_APB2RSTR_ADCRST            ((uint32_t)0x00000100)
#define RCC_APB2RSTR_SDIORST           ((uint32_t)0x00000800)
#define RCC_APB2RSTR_SPI1RST           ((uint32_t)0x00001000)
#define RCC_APB2RSTR_SYSCFGRST         ((uint32_t)0x00004000)
#define RCC_APB2RSTR_TIM9RST           ((uint32_t)0x00010000)
#define RCC_APB2RSTR_TIM10RST          ((uint32_t)0x00020000)
#define RCC_APB2RSTR_TIM11RST          ((uint32_t)0x00040000)

/* Bit fields of the RCC_AHB1ENR register */
#define RCC_AHB1ENR_GPIOAEN            ((uint32_t)0x00000001)
#define RCC_AHB1ENR_GPIOBEN            ((uint32_t)0x00000002)
#define RCC_AHB1ENR_GPIOCEN            ((uint32_t)0x00000004)
#define RCC_AHB1ENR_GPIODEN            ((uint32_t)0x00000008)
#define RCC_AHB1ENR_GPIOEEN            ((uint32_t)0x00000010)
#define RCC_AHB1ENR_GPIOFEN            ((uint32_t)0x00000020)
#define RCC_AHB1ENR_GPIOGEN            ((uint32_t)0x00000040)
#define RCC_AHB1ENR_GPIOHEN            ((uint32_t)0x00000080)
#define RCC_AHB1ENR_GPIOIEN            ((uint32_t)0x00000100)
#define RCC_AHB1ENR_CRCEN              ((uint32_t)0x00001000)
#define RCC_AHB1ENR_BKPSRAMEN          ((uint32_t)0x00040000)
#define RCC_AHB1ENR_CCMDATARAMEN       ((uint32_t)0x00100000)
#define RCC_AHB1ENR_DMA1EN             ((uint32_t)0x00200000)
#define RCC_AHB1ENR_DMA2EN             ((uint32_t)0x00400000)
#define RCC_AHB1ENR_ETHMACEN           ((uint32_t)0x02000000)
#define RCC_AHB1ENR_ETHMACTXEN         ((uint32_t)0x04000000)
#define RCC_AHB1ENR_ETHMACRXEN         ((uint32_t)0x08000000)
#define RCC_AHB1ENR_ETHMACPTPEN        ((uint32_t)0x10000000)
#define RCC_AHB1ENR_OTGHSEN            ((uint32_t)0x20000000)
#define RCC_AHB1ENR_OTGHSULPIEN        ((uint32_t)0x40000000)

/* Bit fields of the RCC_AHB2ENR register */
#define RCC_AHB2ENR_DCMIEN             ((uint32_t)0x00000001)
#define RCC_AHB2ENR_CRYPEN             ((uint32_t)0x00000010)
#define RCC_AHB2ENR_HASHEN             ((uint32_t)0x00000020)
#define RCC_AHB2ENR_RNGEN              ((uint32_t)0x00000040)
#define RCC_AHB2ENR_OTGFSEN            ((uint32_t)0x00000080)

/* Bit fields of the RCC_AHB3ENR register */
#define RCC_AHB3ENR_FSMCEN             ((uint32_t)0x00000001)

/* Bit fields of the RCC_APB1ENR register */
#define RCC_APB1ENR_TIM2EN             ((uint32_t)0x00000001)
#define RCC_APB1ENR_TIM3EN             ((uint32_t)0x00000002)
#define RCC_APB1ENR_TIM4EN             ((uint32_t)0x00000004)
#define RCC_APB1ENR_TIM5EN             ((uint32_t)0x00000008)
#define RCC_APB1ENR_TIM6EN             ((uint32_t)0x00000010)
#define RCC_APB1ENR_TIM7EN             ((uint32_t)0x00000020)
#define RCC_APB1ENR_TIM12EN            ((uint32_t)0x00000040)
#define RCC_APB1ENR_TIM13EN            ((uint32_t)0x00000080)
#define RCC_APB1ENR_TIM14EN            ((uint32_t)0x00000100)
#define RCC_APB1ENR_WWDGEN             ((uint32_t)0x00000800)
#define RCC_APB1ENR_SPI2EN             ((uint32_t)0x00004000)
#define RCC_APB1ENR_SPI3EN             ((uint32_t)0x00008000)
#define RCC_APB1ENR_USART2EN           ((uint32_t)0x00020000)
#define RCC_APB1ENR_USART3EN           ((uint32_t)0x00040000)
#define RCC_APB1ENR_UART4EN            ((uint32_t)0x00080000)
#define RCC_APB1ENR_UART5EN            ((uint32_t)0x00100000)
#define RCC_APB1ENR_I2C1EN             ((uint32_t)0x00200000)
#define RCC_APB1ENR_I2C2EN             ((uint32_t)0x00400000)
#define RCC_APB1ENR_I2C3EN             ((uint32_t)0x00800000)
#define RCC_APB1ENR_CAN1EN             ((uint32_t)0x02000000)
#define RCC_APB1ENR_CAN2EN             ((uint32_t)0x04000000)
#define RCC_APB1ENR_PWREN              ((uint32_t)0x10000000)
#define RCC_APB1ENR_DACEN              ((uint32_t)0x20000000)

/* Bit fields of the RCC_APB2ENR register */
#define RCC_APB2ENR_TIM1EN             ((uint32_t)0x00000001)
#define RCC_APB2ENR_TIM8EN             ((uint32_t)0x00000002)
#define RCC_APB2ENR_USART1EN           ((uint32_t)0x00000010)
#define RCC_APB2ENR_USART6EN           ((uint32_t)0x00000020)
#define RCC_APB2ENR_ADC1EN             ((uint32_t)0x00000100)
#define RCC_APB2ENR_ADC2EN             ((uint32_t)0x00000200)
#define RCC_APB2ENR_ADC3EN             ((uint32_t)0x00000400)
#define RCC_APB2ENR_SDIOEN             ((uint32_t)0x00000800)
#define RCC_APB2ENR_SPI1EN             ((uint32_t)0x00001000)
#define RCC_APB2ENR_SYSCFGEN           ((uint32_t)0x00004000)
#define RCC_APB2ENR_TIM9EN             ((uint32_t)0x00010000)
#define RCC_APB2ENR_TIM10EN            ((uint32_t)0x00020000)
#define RCC_APB2ENR_TIM11EN            ((uint32_t)0x00040000)

/* Bit fields of the RCC_SSCGR register */
#define RCC_SSCGR_MODPER               ((uint32_t)0x00001FFF)
#define RCC_SSCGR_INCSTEP              ((uint32_t)0x0FFFE000)
#define RCC_SSCGR_SPREADSEL            ((uint32_t)0x40000000)
#define RCC_SSCGR_SSCGEN               ((uint32_t)0x80000000)

/* Bit fields of the USART_SR register */
#define USART_SR_PE                    ((uint16_t)0x0001)
#define USART_SR_FE                    ((uint16_t)0x0002)
#define USART_SR_NF                    ((uint16_t)0x0004)
#define USART_SR_ORE                   ((uint16_t)0x0008)
#define USART_SR_IDLE                  ((uint16_t)0x0010)
#define USART_SR_RXNE                  ((uint16_t)0x0020)
#define USART_SR_TC                    ((uint16_t)0x0040)
#define USART_SR_TXE                   ((uint16_t)0x0080)
#define USART_SR_LBD                   ((uint16_t)0x0100)
#define USART_SR_CTS                   ((uint16_t)0x0200)

/* Bit fields of the USART_DR register */
#define USART_DR_DR                    ((uint16_t)0x01FF)

/* Bit fields of the USART_BRR register */
#define USART_BRR_DIV_Fraction         ((uint16_t)0x000F)
#define USART_BRR_DIV_Mantissa         ((uint16_t)0xFFF0)

/* Bit fields of the USART_CR1 register */
#define USART_CR1_SBK                  ((uint16_t)0x0001)
#define USART_CR1_RWU                  ((uint16_t)0x0002)
#define USART_CR1_RE                   ((uint16_t)0x0004)
#define USART_CR1_TE                   ((uint16_t)0x0008)
#define USART_CR1_IDLEIE               ((uint16_t)0x0010)
#define USART_CR1_RXNEIE               ((uint16_t)0x0020)
#define USART_CR1_TCIE                 ((uint16_t)0x0040)
#define USART_CR1_TXEIE                ((uint16_t)0x0080)
#define USART_CR1_PEIE                 ((uint16_t)0x0100)
#define USART_CR1_PS                   ((uint16_t)0x0200)
#define USART_CR1_PCE                  ((uint16_t)0x0400)
#define USART_CR1_WAKE                 ((uint16_t)0x0800)
#define USART_CR1_M                    ((uint16_t)0x1000)
#define USART_CR1_UE                   ((uint16_t)0x2000)
#define USART_CR1_OVER8                ((uint16_t)0x8000)

/* Bit fields of the USART_CR2 register */
#define USART_CR2_ADD                  ((uint16_t)0x000F)
#define USART_CR2_LBDL                 ((uint16_t)0x0020)
#define USART_CR2_LBDIE                ((uint16_t)0x0040)
#define USART_CR2_LBCL                 ((uint16_t)0x0100)
#define USART_CR2_CPHA                 ((uint16_t)0x0200)
#define USART_CR2_CPOL                 ((uint16_t)0x0400)
#define USART_CR2_CLKEN                ((uint16_t)0x0800)
#define USART_CR2_STOP                 ((uint16_t)0x3000)
#define USART_CR2_LINEN                ((uint16_t)0x4000)

/* Bit fields of the USART_CR3 register */
#define USART_CR3_EIE                  ((uint16_t)0x0001)
#define USART_CR3_IREN                 ((uint16_t)0x0002)
#define USART_CR3_IRLP                 ((uint16_t)0x0004)
#define USART_CR3_HDSEL                ((uint16_t)0x0008)
#define USART_CR3_NACK                 ((uint16_t)0x0010)
#define USART_CR3_SCEN                 ((uint16_t)0x0020)
#define USART_CR3_DMAR                 ((uint16_t)0x0040)
#define USART_CR3_DMAT                 ((uint16_t)0x0080)
#define USART_CR3_RTSE                 ((uint16_t)0x0100)
#define USART_CR3_CTSE                 ((uint16_t)0x0200)
#define USART_CR3_CTSIE                ((uint16_t)0x0400)
#define USART_CR3_ONEBIT               ((uint16_t)0x0800)

/* Bit fields of the USART_GTPR register */
#define USART_GTPR_PSC                 ((uint16_t)0x00FF)
#define USART_GTPR_GT                  ((uint16_t)0xFF00)


#ifdef __cplusplus
} /* extern "C" */
#endif

#endif /* PERIDEF_STM32F4XX_H */
