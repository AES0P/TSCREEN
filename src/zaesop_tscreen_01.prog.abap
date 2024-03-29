*MIT License
*
*Copyright (c) 2021 AES0P
*
*Permission is hereby granted, free of charge, to any person obtaining a copy
*of this software and associated documentation files (the "Software"), to deal
*in the Software without restriction, including without limitation the rights
*to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
*copies of the Software, and to permit persons to whom the Software is
*furnished to do so, subject to the following conditions:
*
*The above copyright notice and this permission notice shall be included in all
*copies or substantial portions of the Software.
*
*THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
*IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
*FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
*AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
*LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
*OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
*SOFTWARE.
*&---------------------------------------------------------------------*
*& 包含               ZAESOP_TSCREEN_01
*&---------------------------------------------------------------------*

REPORT zaesop_tscreen_01.

*------------------------TSCREEN框架使用说明及规约---------------------*
*
*     AUTHOR AESOP
*     2021/01/30
*
*   简介：
*       TSCREEN框架（后简称TSCREEN)基于SAP ABAP语言，结合多种OOP设计模式进行开发，
*       是为解决实施过程中重复造轮子、开发风格混乱和技术规范不统一等问题而诞生的。
*
*       基于TSCREEN框架开发的程序，可以统计其相关的运行情况，并且可以使用框架
*       提供的统一日志记录工具进行日志记录，可有效提升开发和运维的工作效率。
*       同时，可以在框架顶层设计中按需定制项目管控措施，而无需具体程序进行修改。
*
*       目前已使用此框架研发出多个高复杂度平台（如采购订单平台、资金票据平台、
*       内外向交货单平台、WMS单据平台及UI5动态生成框架等），在多个（超）大型集
*       团/企业中投入实际生产使用并平稳运行。
*
*   设计初衷：
*       1、统一大中小平台的开发风格和技术规范，降低新手技术顾问开发平台的门槛
*       2、减少自建屏幕逻辑流的差异性，将每个屏幕流事件转换为对应的事件方法，
*       将POV&POH中“一个字段需要写一个MODULE”的传统改为使用统一事件处理入口
*       3、将常用的SAP标准功能进行封装，融入TSCREEN框架中，同时支持开发者通过
*       实现接口，自行开发新的TSCREEN控件
*       4、遵循SAP 2020年在GITHUB发布的简洁编程开发规范指导，可使用ABAPGIT在任
*       何已有ABAPGIT的环境进行安装及使用
*       5、使用视图栈自动管理每一个TSCREEN对象，开发者无需主动去创建、调用和销
*       毁TSCREEN对象，从而能够将更多的精力放在具体业务的实现上
*
*    使用说明和规约：
*       1、TSCREEN对报表程序事件的控制，参考DEMO ZAESOP_TSCREEN_02。引入TSCREEN
*       的步骤：
*       ①程序中有且仅有1个继承于ZCL_TREPORT的本地类LCL_PROG作为报表控制类
*       ②LCL_PROG中必须存在类方法PUSH_VIEW，以实现视图自动入栈机制（方法无需自己实现，
*       直接从DEMO程序中COPY即可）
*       ③引入通用头文件：ZAESOP_TSCREEN_EVENT_INC，以实现视图自动出栈机制和事件响应机制
*       ④按需实现继承于ZCL_TSCREEN和ZCL_TSCREEN_WITH_COMPONENTS的视图逻辑（参考DEMO03-17)
*       其子类命名建议使用统一的前缀，参考DEMO03中LCL_PROG的类变量VIEW_PREFIX的使用
*
*       2、实施时，报表程序自身，和每一个自建屏幕，都应被视为一个个独立的对象去处理。
*       每个对象不应跨越自身的职责去处理不相干的业务逻辑，以保证高内聚和低耦合。
*
*       3、每一个内建控件都实现了ZIF_TSCREEN_COMPONENT接口，即如果有新的控件想要
*       加入到TSCREEN中，先实现该接口，再参考内建控件的 构造函数 代码去实现，即可
*       自动被TSCREEN识别和管控
*
*       4、T-CODE
*       ① ZTSCREEN01 可实现对TSCREEN中所有屏幕元素的属性（只读、可编辑、必输
*       和隐藏）控制，只需要输入正确的屏幕元素信息（如所在程序名、屏幕编号、所在控件
*       及名称等）即可。建议必输属性在本地类中进行控制，否则可能因为标准功能的限制，
*       给用户带来不好的体验。如果要根据单据的不同状态实现不同的属性控制，可参考此功
*       能自己建配置表，再重写PBO方法即可。按钮的功能码和名称最好一致，可以免去打开
*       自建屏幕查看名称的麻烦。
*       ② ZTSCREEN02 程序运行日志报表。通过此报表可以统计程序的运行状况和使用最多的功能。
*
*       5.TABLE CONTROL控件的名称必须形如 TC_9000_01（代表9000屏幕第一个TC，若只存在
*       一个TC，可以其它形式命名，但不推荐），其相关按钮的功能码，都必须带上TABLE CONTROL
*       控件的名称，如TC_9000_01_DELE，否则无法被对应的TC控件识别。
*
*       6、TC若要响应双击事件，请将GUI状态的F2点击事件功能码设为"PICK"，这样双击之后就能
*       获取到对应的字段名。
*
*
*------------------------TSCREEN框架使用说明及规约----------------------*
