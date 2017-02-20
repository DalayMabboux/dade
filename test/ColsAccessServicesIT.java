package com.swisscom.esw.cols.ws;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.List;
import java.util.SortedSet;
import java.util.TreeSet;

import javax.xml.datatype.DatatypeFactory;

import org.junit.Assert;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import com.itservices.servicedb.cols.ws.physical.ProcessMsgCPEOrderingOperationRequest;
import com.itservices.servicedb.common.attribute.Constants;
import com.itservices.servicedb.common.exception.ServiceDbRuntimeException;
import com.itservices.servicedb.common.util.AttributeManagerUtil;
import com.itservices.servicedb.common.util.ObjectUtil;
import com.itservices.servicedb.ejb.facade.BPerData;
import com.itservices.servicedb.eoe.EoeOrderUtils;
import com.itservices.servicedb.eoe.Order.OrderState;
import com.itservices.servicedb.eoe.OrderDetails;
import com.itservices.servicedb.eoe.OrderObjectEvaluator.OrderObjectType;
import com.itservices.servicedb.eoe.OrderX;
import com.itservices.servicedb.itsm.ws.ManageChangeRequest;
import com.itservices.servicedb.lcs.eoe.LcsNotificationHandler;
import com.itservices.servicedb.lcs.ws.notify.NotificationType;
import com.itservices.servicedb.lcs.ws.order.OrderType;
import com.itservices.servicedb.util.ErrorInfoUtil;
import com.swisscom.esw.cols.ColsConstants;
import com.swisscom.esw.cols.entity.ISystemInteractionEntry;
import com.swisscom.esw.cols.entity.NePort;
import com.swisscom.esw.cols.entity.UpData;
import com.swisscom.esw.cols.entity.access.ColsAccess;
import com.swisscom.esw.cols.entity.access.ColsBaseConnectivity;
import com.swisscom.esw.cols.entity.access.MscHeLeg;
import com.swisscom.esw.cols.entity.access.MscLayer;
import com.swisscom.esw.cols.entity.access.MscLowEndLeg;
import com.swisscom.esw.cols.entity.connectivity.L2ServiceConnectivity;
import com.swisscom.esw.cols.entity.connectivity.L3ServiceConnectivity;
import com.swisscom.esw.cols.entity.connectivity.MediantPortName;
import com.swisscom.esw.cols.entity.cpe.ColsCpeModel;
import com.swisscom.esw.cols.entity.cpe.Cpe;
import com.swisscom.esw.cols.entity.cpe.CpeLan;
import com.swisscom.esw.cols.entity.cpe.CpeMaterial;
import com.swisscom.esw.cols.entity.cpe.CpeMaterialUse;
import com.swisscom.esw.cols.entity.cpe.MediantModel;
import com.swisscom.esw.cols.entity.cpe.MediantSlot;
import com.swisscom.esw.cols.entity.embeddable.OrderHeaderDataNG;
import com.swisscom.esw.cols.entity.erroneousOrder.ErroneousOrder;
import com.swisscom.esw.cols.entity.history.update.AttributeUpdate;
import com.swisscom.esw.cols.entity.history.update.ColsUpdateHistory;
import com.swisscom.esw.cols.entity.leadtime.ColsLeadTime;
import com.swisscom.esw.cols.entity.net.L2Net;
import com.swisscom.esw.cols.entity.net.P2PNet;
import com.swisscom.esw.cols.entity.order.ColsNorthboundOrder;
import com.swisscom.esw.cols.entity.process.access.ColsProcessAccessConfirm;
import com.swisscom.esw.cols.entity.process.access.ColsProcessAccessCreateHighEnd;
import com.swisscom.esw.cols.entity.process.access.ColsProcessAccessCreateLowEnd;
import com.swisscom.esw.cols.entity.process.access.ColsProcessAccessDelete;
import com.swisscom.esw.cols.entity.process.access.ColsProcessAccessUpdate;
import com.swisscom.esw.cols.entity.process.connectivity.ColsProcessConnectivityPortMoveCanMcan;
import com.swisscom.esw.cols.entity.process.connectivity.ColsProcessConnectivityPortMovePhysicalCpe;
import com.swisscom.esw.cols.entity.request.ColsRequestNG;
import com.swisscom.esw.cols.entity.request.access.ColsRequestAccessUpdate;
import com.swisscom.esw.cols.entity.request.connectivity.ColsRequestConnectivityPortMoveCanMcan;
import com.swisscom.esw.cols.entity.request.connectivity.ColsRequestConnectivityPortMovePhysicalCpe;
import com.swisscom.esw.cols.entity.response.access.ColsResponseAccessCreateHighEnd;
import com.swisscom.esw.cols.entity.response.access.ColsResponseAccessCreateLowEnd;
import com.swisscom.esw.cols.entity.response.access.ColsResponseAccessUpdate;
import com.swisscom.esw.cols.entity.response.connectivity.ColsResponseConnectivityPortMoveCanMcan;
import com.swisscom.esw.cols.entity.response.connectivity.ColsResponseConnectivityPortMovePhysicalCpe;
import com.swisscom.esw.cols.entity.response.net.ColsResponseNetCreateL2;
import com.swisscom.esw.cols.entity.site.ColsSite;
import com.swisscom.esw.cols.entity.user.ColsUser;
import com.swisscom.esw.cols.intermediateinfo.ws.IntermediateInformationService;
import com.swisscom.esw.cols.intermediateinfo.ws.IntermediateInformation_Type.Notification;
import com.swisscom.esw.cols.leadtime.ColsOrderCategory;
import com.swisscom.esw.cols.order.ColsOrderType;
import com.swisscom.esw.cols.order.service.ColsOrderAlarmActivationService;
import com.swisscom.esw.cols.order.service.ColsTimeSlot;
import com.swisscom.esw.cols.order.service.EoeOrderServiceCols;
import com.swisscom.esw.cols.order.service.EoeOrderServiceColsBean;
import com.swisscom.esw.cols.repository.ColsAccessRepository;
import com.swisscom.esw.cols.repository.L3ServiceConnectivityRepository;
import com.swisscom.esw.cols.repository.history.update.ColsUpdateHistoryRepository;
import com.swisscom.esw.cols.repository.leadtime.ColsLeadTimeRepository;
import com.swisscom.esw.cols.repository.order.ColsOrderRepository;
import com.swisscom.esw.cols.repository.process.connectivity.ColsProcessConnectivityPortMoveCanMcanRepository;
import com.swisscom.esw.cols.service.access.ColsAccessService;
import com.swisscom.esw.cols.service.connectivity.L2ServiceConnectivityService;
import com.swisscom.esw.cols.service.connectivity.L3ServiceConnectivityService;
import com.swisscom.esw.cols.service.crud.CrudService;
import com.swisscom.esw.cols.service.erroneousOrder.ErroneousOrderService;
import com.swisscom.esw.cols.service.exceptions.ObjectNotFoundException;
import com.swisscom.esw.cols.service.order.ColsNorthboundOrderService;
import com.swisscom.esw.cols.service.order.sender.SendOeColsOrder;
import com.swisscom.esw.cols.service.process.access.CreateColsAccessLowEndProcessService;
import com.swisscom.esw.cols.service.process.access.CreateColsAccessProcessService;
import com.swisscom.esw.cols.service.process.access.UpdateColsAccessProcessService;
import com.swisscom.esw.cols.service.systemInteraction.SystemInteractionEntryService;
import com.swisscom.esw.cols.ses.OoeOrderColsFacadeIT;
import com.swisscom.esw.cols.sim.ColsWfmSimulator;
import com.swisscom.esw.cols.ui.om.OrderDetailPm;
import com.swisscom.esw.cols.util.CopeLeadTimeCalc;
import com.swisscom.esw.cols.util.OrderCreation;
import com.swisscom.esw.cols.webclient.service.exceptions.BusinessException;
import com.swisscom.esw.cols.webservice.service.NorthboundWebService;
import com.swisscom.esw.cols.ws.order.AttributeType;
import com.swisscom.esw.cols.ws.order.Order;
import com.swisscom.esw.cols.ws.query.ColsRequest;
import com.swisscom.esw.cols.ws.query.ColsRequest.Arguments.Argument;
import com.swisscom.esw.cols.ws.query.ColsResponse;
import com.swisscom.esw.cols.ws.tibco.notify.UpdateOrderStatusRequestType;
import com.swisscom.esw.dev.Todo.Developer;
import com.swisscom.esw.dev.Todo.TodoFixTest;
import com.swisscom.esw.entity.EswLog;
import com.swisscom.esw.entity.EswLogRepository;
import com.swisscom.esw.entity.attribute.AttributeValue;
import com.swisscom.esw.entity.attribute.AttributeValueRepository;
import com.swisscom.esw.entity.eoe.OeOrderRepository;
import com.swisscom.esw.eoe.EoeOrderHandler;
import com.swisscom.esw.eoe.ExecutionContextEoe.NotificationSimulationType;
import com.swisscom.esw.lib.ServiceLocatorCdi;
import com.swisscom.esw.lib.util.EswDateUtils;
import com.swisscom.esw.qualifier.QualifierUtils;
import com.swisscom.esw.test.junit.ColsContainerTest;
import com.swisscom.esw.util.MessageHolder;

import ch.inftec.ju.testing.db.DataSet;
import ch.inftec.ju.testing.db.DataSetConfig;
import ch.inftec.ju.testing.db.DataSetExport;
import ch.inftec.ju.testing.db.DataSetExport.ExportType;
import ch.inftec.ju.util.IOUtil;
import ch.inftec.ju.util.JuStringUtils;
import ch.inftec.ju.util.JuUrl;
import ch.inftec.ju.util.xml.XmlUtils;

/**
 * All ColsAccess related End-to-End & UI-Setup Tests
 * 
 * @author Jogin Joy <jogin.joy@inftec.ch>
 * 
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
class ColsAccessServicesIT extends ColsServiceCommonBehaviorTest {

	/**
	 * Gets a new instance of this test class that can be used by other tests that need test results
	 * from here as input for their tests.
	 * 
	 * @return
	 */
	public static ColsAccessServicesIT getInstance(ColsContainerTest hostTest) {
		ColsAccessServicesIT colsServiceOrderIT = new ColsAccessServicesIT();
		colsServiceOrderIT.init();
		colsServiceOrderIT.initFrom(hostTest);
		return colsServiceOrderIT;
	}

	/**
	 * Create in DB a Cols LeadTime
	 */
	private void createColsLeadTimes() {
		AttributeValueRepository attributeValueRepository = ServiceLocatorCdi.cdi(AttributeValueRepository.class);

		AttributeValue deleteColsAccessAttributeValue = attributeValueRepository
				.findByAttributeTypeIdAndAttributeValueId(1552L, 155200023L).get(0);

		ColsLeadTimeRepository colsLeadTimeRepository = ServiceLocatorCdi.cdi(ColsLeadTimeRepository.class, QualifierUtils.COLS_ANNO);

		ColsLeadTime colsLeadTime = new ColsLeadTime();
		colsLeadTime.setActionDate(1);
		colsLeadTime.setLeadTime(2);
		colsLeadTime.setMessage("message1");
		colsLeadTime.setOrderType(deleteColsAccessAttributeValue);
		colsLeadTime.setColsOrderCategory(ColsOrderCategory.DELETE_COLS_ACCESS);
		colsLeadTimeRepository.save(colsLeadTime);
		debug().commitToDb();
	}

	/**
	 * Method that cleans up the DB
	 * <p>
	 * This class executes methods in ascending order of the method names. And this method makes sure, as this executes last, that the DB is
	 * in a clean state after execution of tests in this class
	 * <p>
	 * Reason: Had problems with left over entries from test cases
	 */
	@Test
	public void zzDBCleanUp() {

	}

	/**
	 * Test that checks the successful update of a CAN/mCAN port move request
	 */
	@TodoFixTest(value = "Should be able to remove DB commit once we have migrated OeOrder to JPA. CanBusinessPortMoveNotificationHandler.canHandle couldn't see process otherwise...", developer = Developer.Martin)
	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/cpeData.xml", "testInputData/bperData_from_dataExport.xml" })
	public void can_updatePortMoveCanMcan() {

		Long portId = 24914981L;
		Long portId2 = 24914985L;

		// create a test colsAccess object
		ColsAccess colsAccess = this.testData().cols().newColsAccess("1", ColsAccessLegType.LOW_END);

		// Set a canName and canPort
		MscLowEndLeg mscLeg = (MscLowEndLeg) colsAccess.getMscLeg();
		mscLeg.getNePort().setNeName("canName");
		mscLeg.getNePort().setNePort("canPort");

		// Create two MscLayers and add it to the Leg
		MscLayer mscLayer1 = new MscLayer();
		mscLayer1.setMscLayerId("mscLayerId1");
		mscLayer1.setCfsId("mscLayerCfsId1");
		mscLayer1.setPortId(portId);

		MscLayer mscLayer2 = new MscLayer();
		mscLayer2.setMscLayerId("mscLayerId2");
		mscLayer2.setCfsId("mscLayerCfsId2");
		mscLayer2.setPortId(portId2);

		mscLeg.add(mscLayer1);
		mscLeg.add(mscLayer2);

		// Persist the ColsAccess
		ColsAccessRepository colsAccessRepo = ServiceLocatorCdi.cdi(ColsAccessRepository.class, QualifierUtils.COLS_ANNO);
		colsAccess = colsAccessRepo.save(colsAccess);

		Assert.assertNotNull("ColsAccess shouldn't be null", colsAccess);
		Assert.assertNotNull("MscLeg shouldn't be null", colsAccess.getMscLeg());
		Assert.assertNotNull("MscLayer shouldn't be null", colsAccess.getMscLeg().getMscLayers());
		Assert.assertEquals("canName", mscLeg.getNePort().getNeName());
		Assert.assertEquals("canPort", mscLeg.getNePort().getNePort());

		Order colsPortMoveNbOrder = this.loadColsOrder("UpdatePortMoveCanMcanNbOrder.xml");

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(colsPortMoveNbOrder);

		OrderX order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", this.getModifiedSetter()));

		// check State
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());

		// Check if the correct notification was sent back to Caller Application once the order was submitted & started at COLS
		this.assertLastNotificationXml("ColsServiceIT_portMoveCanMcan_inProgress_notification.xml");

		// This should have triggered the cols order and should have invoked the action class for the business port that send the order to
		// the NC
		// Simulate the notification from the NC - it calls the ColsService.callService() to Notify the status back to COLS
		ColsRequest colsPortMoveNbNotify = this.loadColsRequest("UpdateBusinessPortMoveCanMcanNbNotify.xml");
		new ColsService().callService(colsPortMoveNbNotify);

		// This should have updated the canName and canPort on the installed base with the new ones from the NC
		Assert.assertEquals("canName", ((MscLowEndLeg) colsAccess.getMscLeg()).getNePort().getNeName());
		Assert.assertEquals("newCanPort", ((MscLowEndLeg) colsAccess.getMscLeg()).getNePort().getNePort());

		// reprocess the order
		// reprocessOrder(order.getOrderId());

		// Check create ColsNorthboundOrder
		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrder = null;
		try {
			nbOrder = nbOrderService.findByOrderId("EOE-0000001");
			Assert.assertNotNull(nbOrder);
		} catch (ObjectNotFoundException e) {
		}
		Assert.assertNotNull(nbOrder);
		Assert.assertEquals(1, nbOrder.getSuborders().size());

		assertion().cope().orderStatus(OrderState.FINISHED, order);

		// Check if the correct notification was sent back to Caller Application once the order was submitted & started at COLS
		this.assertLastNotificationXml("ColsServiceIT_portMoveCanMcan_finished_notification.xml");

		ColsUpdateHistoryRepository updateHistoryRepo = ServiceLocatorCdi.cdi(ColsUpdateHistoryRepository.class, QualifierUtils.COLS_ANNO);
		List<ColsUpdateHistory> updates = updateHistoryRepo.findByColsEntityId(colsAccess.getColsAccessId());
		assertEquals(1, updates.size());
		SortedSet<AttributeUpdate> attributes = updates.get(0).getAttributes();
		assertEquals(1, attributes.size());
		assertEquals("colsAccessPoId-1", updates.get(0).getColsEntityId());
		assertEquals("ITSM", updates.get(0).getChangeUser());
		assertEquals("NodePort", attributes.first().getAttributeName());
		assertEquals("canPort", attributes.first().getOldValue());
		assertEquals("newCanPort", attributes.first().getNewValue());
	}

	/**
	 * Test that checks the successful update of a CAN/mCAN port move request
	 */
	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/cpeData.xml", "testInputData/bperData_from_dataExport.xml" })
	public void can_updatePortMoveCanMcanWithBperAssign() {

		String neName = "xxx-xxx620-x-xx-99";
		Long portId = 24914981L;
		Long portId2 = 24914985L;

		// create a test colsAccess object
		ColsAccess colsAccess = this.testData().cols().newColsAccess("1", ColsAccessLegType.LOW_END);
		colsAccess.getLogicalPhysicalCpe().getCpe().setCpeName("ipd-lss180-r-pe-02");
		colsAccess.setPortId(portId);

		// Set a canName and canPort
		MscLowEndLeg mscLeg = (MscLowEndLeg) colsAccess.getMscLeg();
		mscLeg.getNePort().setNeName(neName);
		mscLeg.getNePort().setNePort("canPort");

		// Create two MscLayers and add it to the Leg
		MscLayer mscLayer1 = new MscLayer();
		mscLayer1.setMscLayerId("mscLayerId1");
		mscLayer1.setCfsId("mscLayerCfsId1");
		mscLayer1.setPortId(portId);

		MscLayer mscLayer2 = new MscLayer();
		mscLayer2.setMscLayerId("mscLayerId2");
		mscLayer2.setCfsId("mscLayerCfsId2");
		mscLayer2.setPortId(portId2);

		mscLeg.add(mscLayer1);
		mscLeg.add(mscLayer2);

		// Persist the ColsAccess
		ColsAccessRepository colsAccessRepo = ServiceLocatorCdi.cdi(ColsAccessRepository.class, QualifierUtils.COLS_ANNO);
		colsAccess = colsAccessRepo.save(colsAccess);

		Assert.assertNotNull("ColsAccess shouldn't be null", colsAccess);
		Assert.assertNotNull("MscLeg shouldn't be null", colsAccess.getMscLeg());
		Assert.assertNotNull("MscLayer shouldn't be null", colsAccess.getMscLeg().getMscLayers());
		Assert.assertEquals(neName, mscLeg.getNePort().getNeName());
		Assert.assertEquals("canPort", mscLeg.getNePort().getNePort());

		L2Net l2Net = this.testData().cols().newL2Net("1", "CES");
		L2ServiceConnectivity l2Connectivity = this.testData().cols().newL2ServiceConnectivity("1", colsAccess, l2Net);

		// set some values into the connectivity attributes that's about to be updated through the order

		l2Connectivity.setCpeLanPortName("cpeLanPortName");
		l2Connectivity.setServiceAccessCfsId1("ServiceIdL2ServiceAccess");

		l2Connectivity.getL2ServiceAccess().getColsAccess().getLogicalPhysicalCpe().getCpe()
				.setCpeLanPortName("cpeLanPortName");
		L2ServiceConnectivityService l2ConnectivityService = ServiceLocatorCdi.cdi(L2ServiceConnectivityService.class);
		l2Connectivity = l2ConnectivityService.update(l2Connectivity);

		ColsBaseConnectivity aConnectivity = new ColsBaseConnectivity();
		aConnectivity.setColsBaseConnectivityId("id-1");
		aConnectivity.setCfsId("cfsId-1");
		aConnectivity.setIspCode("ispCode-1");
		aConnectivity.setCreationType("creationType-1");
		aConnectivity.setDnVnNsn("dnVnNsn-1");
		aConnectivity.setDslamName("bsw620");

		colsAccess.setColsBaseConnectivity(aConnectivity);

		Order colsPortMoveNbOrder = this.loadColsOrder("UpdatePortMoveCanMcanNbOrder.xml");

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(colsPortMoveNbOrder);

		OrderX order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", this.getModifiedSetter()));
		OrderX orderAssignBper = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.001", this.getModifiedSetter()));
		OrderX orderUpdateMcan = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.002", this.getModifiedSetter()));

		// check State
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, orderAssignBper.getState());
		Assert.assertEquals(OrderState.WAITING, orderUpdateMcan.getState());

		// Check if the correct notification was sent back to Caller Application once the order was submitted & started at COLS
		this.assertLastNotificationXml("ColsServiceIT_portMoveCanMcan_inProgress_notification.xml");

		// Here we should set the required BPER data so the order can go on.
		// Check create ColsNorthboundOrder
		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrder = null;
		try {
			nbOrder = nbOrderService.findByOrderId("EOE-0000001");
			Assert.assertNotNull(nbOrder);
		} catch (ObjectNotFoundException e) {
		}

		ColsProcessConnectivityPortMoveCanMcan aProcess = (ColsProcessConnectivityPortMoveCanMcan) nbOrder.getColsProcess();

		boolean isDual = false;
		MessageHolder msg = new MessageHolder();
		String user = "charlie";
		EoeOrderServiceCols eoeOrderServiceCols = this.serviceLocator.cdi(EoeOrderServiceCols.class);
		BPerData bper = eoeOrderServiceCols.getBperDataForUpdate("EOE-0000001", getMetaData(), user, neName, isDual, msg);
		Assert.assertNotNull("bper may not be empty", bper);
		eoeOrderServiceCols.assignBper("EOE-0000001", bper.getPortId(), getMetaData(), false);

		ColsProcessConnectivityPortMoveCanMcanRepository updatePortMoveRepo = ServiceLocatorCdi.cdi(
				ColsProcessConnectivityPortMoveCanMcanRepository.class,
				QualifierUtils.COLS_ANNO);
		updatePortMoveRepo.save(aProcess);

		ServiceLocatorCdi.cdi(EoeOrderHandler.class).queueForProcessing(order, null);

		// check State
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, orderAssignBper.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, orderUpdateMcan.getState());

		// This should have triggered the cols order and should have invoked the action class for the business port that send the order to
		// the NC
		// Simulate the notification from the NC - it calls the ColsService.callService() to Notify the status back to COLS
		ColsRequest colsPortMoveNbNotify = this.loadColsRequest("UpdateBusinessPortMoveCanMcanNbNotify.xml");
		new ColsService().callService(colsPortMoveNbNotify);

		// This should have updated the canName and canPort on the installed base with the new ones from the NC
		Assert.assertEquals("canName", ((MscLowEndLeg) colsAccess.getMscLeg()).getNePort().getNeName());
		Assert.assertEquals("newCanPort", ((MscLowEndLeg) colsAccess.getMscLeg()).getNePort().getNePort());

		// check State
		Assert.assertEquals(OrderState.FINISHED, order.getState());
		Assert.assertEquals(OrderState.FINISHED, orderAssignBper.getState());
		Assert.assertEquals(OrderState.FINISHED, orderUpdateMcan.getState());

		try {
			nbOrder = nbOrderService.findByOrderId("EOE-0000001");
			Assert.assertNotNull(nbOrder);
		} catch (ObjectNotFoundException e) {
		}
		Assert.assertNotNull(nbOrder);
		Assert.assertEquals(2, nbOrder.getSuborders().size());

		assertion().cope().orderStatus(OrderState.FINISHED, order);

		// Check if the correct notification was sent back to Caller Application once the order was submitted & started at COLS
		this.assertLastNotificationXml("ColsServiceIT_portMoveCanMcan_finished_notification.xml");

		ColsUpdateHistoryRepository updateHistoryRepo = ServiceLocatorCdi.cdi(ColsUpdateHistoryRepository.class, QualifierUtils.COLS_ANNO);
		List<ColsUpdateHistory> updates = updateHistoryRepo.findByColsEntityId(colsAccess.getColsAccessId());
		assertEquals(1, updates.size());
		SortedSet<AttributeUpdate> attributes = updates.get(0).getAttributes();
		assertEquals(2, attributes.size());

		assertEquals("colsAccessPoId-1", updates.get(0).getColsEntityId());
		assertEquals("ITSM", updates.get(0).getChangeUser());
		assertEquals("NodeName", attributes.first().getAttributeName());
		assertEquals("xxx-xxx620-x-xx-99", attributes.first().getOldValue());
		assertEquals("canName", attributes.first().getNewValue());

		assertEquals("NodePort", attributes.last().getAttributeName());
		assertEquals("canPort", attributes.last().getOldValue());
		assertEquals("newCanPort", attributes.last().getNewValue());
	}

	@Test
	public void canCreate_accessHighEnd_withMultiplePlug() throws ObjectNotFoundException {

		Order newColsOrder = OrderCreation.createNewAccessOrder();

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(newColsOrder);

		// Get the order that was implicitly created by the ColsService
		OrderX preOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", this.getModifiedSetter()));

		// Get SubOrders we will make checks on
		OrderX s33PreOrderTibcoOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_S33_PRE_ORDER.getType());
		OrderX n20PreOrderConfirmedOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_N20_PRE_ORDER_CONFIRMED.getType());

		// Process the order to make sure it's processed without errors
		OrderDetails od = preOrder.process();
		Assert.assertFalse(od.isHasErrors());

		// Make sure the S33 suborder is waiting (for the CPE Model assignment)
		Assert.assertEquals(OrderState.WAITING, s33PreOrderTibcoOrder.getState());

		// The SubOrder COLS_ORDER_ASSIGN_CPE should be in state READY
		OrderX assignCpeOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE.getType());
		Assert.assertEquals(OrderState.IN_PROGRESS, assignCpeOrder.getState());

		/**** SET CPE MULTIPLE PLUG ***/
		// Simulate the GUI Assign of CPE Data with Multiple Plug. GUI calls same service as below:
		setCpeDataWithMultiplePlug(preOrder, false);

		Assert.assertEquals(OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getState());

		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_n20_withMultiplePlug_copeOrder.xml");

		// Check Main Order ready notification XML
		this.assertLastNotificationXml("ColsServiceIT_n20_main_ready_notification.xml");

		// The main order should be in progress
		Assert.assertEquals(OrderState.IN_PROGRESS, preOrder.getState());

		// S33 Preorder should be in progress, waiting for manual update of the data
		Assert.assertEquals(OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getState());

		// ConfirmPreOrder should be waiting as we don't have the necessary data to confirm/cancel yet
		Assert.assertEquals(OrderState.WAITING, n20PreOrderConfirmedOrder.getState());
		// Phase Id should already be in progress
		OrderHeaderDataNG ohd = preOrder
				.getDataObjectNotNull(ColsProcessAccessCreateHighEnd.class, true).getRequest()
				.getOrderHeaderData();
		Assert.assertEquals(Constants.COLS_ORDER_PHASE_PREORDER_IN_PROGRESS, ohd.getPhaseId());

		// Simulate notification from SouthBound
		OrderType submittedOrder = this.pollLastRequest(OrderType.class);

		// First, CoPE will send an intermediate notification of type SubmitOrderAck with status succeeded,
		// containing one OrderLineItem result for the PO_MSC_HIGH_END_LEG with status succeeded, containing the attribute
		// circuitId.
		// As soon as the circuitId is known, we need to set it on the responses object and send a notification to CFU.

		new LcsNotificationHandler().simulateNotification(submittedOrder, NotificationType.SUBMIT_ORDER_ACK);

		// Make sure intermediate notification was sent
		// this.assertion().cope().lastNotificationStati("1234", "in progress", "in progress", "preorder");

		// Make sure the circuitId was stored to the responses object
		ColsResponseAccessCreateHighEnd response = preOrder.getDataObjectNotNull(
				ColsProcessAccessCreateHighEnd.class, true)
				.getResponse();
		Assert.assertNotNull(response.getCircuitId());

		new LcsNotificationHandler().simulateNotification(submittedOrder);

		// Check Pre Order ready notification XML
		this.assertLastNotificationXml("ColsServiceIT_n20_preorder_withMultiplePlug_ready_notification.xml");

		// S33 PreOrder should be in progress now, phase still in progress
		Assert.assertEquals(OrderState.IN_PROGRESS, n20PreOrderConfirmedOrder.getState());
		Assert.assertEquals(Constants.COLS_ORDER_PHASE_PREORDER_IN_PROGRESS, ohd.getPhaseId());

		ColsAccessRepository colsAccessRepository = ServiceLocatorCdi.cdi(ColsAccessRepository.class, QualifierUtils.COLS_ANNO);
		// There should be no access created since this is the preordering
		assertEquals(0, colsAccessRepository.count());

		// Check cpe response values
		response = preOrder.getDataObjectNotNull(ColsProcessAccessCreateHighEnd.class, true)
				.getResponse();
		Assert.assertNotNull(response.getCpe().getCpeModel());
		Assert.assertNotNull(response.getCpe().getCpeName());

		// CPE WAN
		Assert.assertNotNull(response.getCpe().getCpeWanPortPluggable());
		Assert.assertNotNull(response.getCpe().getCpeWanPortName());

		// CPE LAN
		Assert.assertEquals(1, response.getCpe().getCpeLanList().first().getPosition());
		Assert.assertEquals("cpeLanPortName1", response.getCpe().getCpeLanList().first().getCpeLanPortName());
		Assert.assertEquals("cpeLanPortPluggable1", response.getCpe().getCpeLanList().first().getCpeLanPortPluggable());
		Assert.assertEquals(5, response.getCpe().getCpeLanList().last().getPosition());
		Assert.assertEquals("cpeLanPortName5", response.getCpe().getCpeLanList().last().getCpeLanPortName());
		Assert.assertEquals("cpeLanPortPluggable5", response.getCpe().getCpeLanList().last().getCpeLanPortPluggable());

		// Confirm this order through the EoeOrderServiceCols

		ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).confirmPreOrder(s33PreOrderTibcoOrder.getOrderId(), getMetaData());

		OrderX n20ConfirmOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002", getModifiedSetter()));
		Assert.assertNotNull(n20ConfirmOrder);

		Assert.assertEquals(OrderState.IN_PROGRESS, n20ConfirmOrder.getSubOrderX(ColsOrderType.COLS_CPE_ORDER_DONE.getType()).getState());

		setCpeOrderDone(n20ConfirmOrder);

		Assert.assertEquals(OrderState.FINISHED, n20ConfirmOrder.getSubOrderX(ColsOrderType.COLS_CPE_ORDER_DONE.getType()).getState());

		// Make sure the confirmation produced a notification
		this.assertLastNotificationXml("ColsServiceIT_n20_withMultiplePlug_preorder_confirmed_notification.xml");

		// Simulate the GUI Assign of CPE Material. GUI calls same service as below:
		OrderX assignCpeMaterialOrder = n20ConfirmOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE_MATERIAL.getType());
		/** SET CPE MATERIALS **/
		setCpeMaterial(assignCpeMaterialOrder);

		setCreateColsAccessOrderTasks(n20ConfirmOrder, getMetaData());

		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrder = nbOrderService.findByOrderId("EOE-0000002");
		ColsProcessAccessConfirm process = (ColsProcessAccessConfirm) nbOrder.getColsProcess();

		// Simulate WFM Response
		int wfmId = Integer.valueOf(process.getResponse().getWfmId());
		ServiceLocatorCdi.cdi(ColsWfmSimulator.class).simulateResponse(wfmId, ColsWfmSimulator.CLOSED);

		// check sent order : to fix the xml
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_n20_copePreOrderConfirm.xml");

		// Main Order should stay in IN_PROGRESS
		Assert.assertEquals("main order should still be in IN_PROGRESS", OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getParentX()
				.getState());

		Assert.assertEquals("confirm order should be in process ", OrderState.IN_PROGRESS, n20ConfirmOrder.getState());

		// Simulate CoPE Notification
		new LcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		// Activate Alarm
		ServiceLocatorCdi.cdi(ColsOrderAlarmActivationService.class).activateAlarm(n20ConfirmOrder.getOrderId());

		Assert.assertEquals("checking n20ConfirmOrder", OrderState.FINISHED, n20ConfirmOrder.getState());
		Assert.assertEquals("confirm order should be finished", OrderState.FINISHED, s33PreOrderTibcoOrder.getState());
		Assert.assertEquals("main order should be ", OrderState.FINISHED, s33PreOrderTibcoOrder.getParentX().getState());
		Assert.assertEquals("main order should be ", OrderState.FINISHED, preOrder.getState());

		// There should be one access created due to the confirmation
		assertEquals(1, colsAccessRepository.count());

		// Check notification XML
		this.assertLastNotificationXml("ColsServiceIT_n20_withMultiplePlug_notification.xml");

		ColsAccessService colsAccessService = ServiceLocatorCdi.cdi(ColsAccessService.class);
		List<ColsAccess> accesses = colsAccessService.searchAll();
		assertEquals(1, accesses.size());

		// Check values
		ColsAccess aColsAccess = accesses.get(0);
		assertEquals("unrelated", aColsAccess.getAccessType());
		assertEquals("123", aColsAccess.getCustomerId());
		assertEquals("HighEndFiber", aColsAccess.getServiceRealizationType());
		assertEquals("HighEnd", aColsAccess.getCpeOption());
		assertEquals("0", aColsAccess.getCustomerWindow());
		assertEquals("ServiceIdLogicalPhysicalCpe", aColsAccess.getLogicalPhysicalCpe().getCfsIdLogicalPhysicalCpe());
		assertEquals("AC0000001", aColsAccess.getAccessId());
		assertEquals("COL:ACC:1", aColsAccess.getColsAccessId());
		assertEquals("ALL:SUB:1", aColsAccess.getMscLeg().getMscLegId());
		assertEquals("ALL:SUB:2", aColsAccess.getLogicalPhysicalCpe().getLogicalPhysicalCpeId());

		// Check Customer values
		assertEquals("UBS AG", aColsAccess.getColsSite().getCompanyName());
		assertEquals("Bahnhofstrasse", aColsAccess.getColsSite().getStreet());
		assertEquals("100", aColsAccess.getColsSite().getHouseNumber());
		assertEquals("Bern", aColsAccess.getColsSite().getCity());
		assertEquals("300800", aColsAccess.getColsSite().getPostalCode());
		assertEquals("Peter Schuhmacher", aColsAccess.getColsSite().getContactPerson());
		assertEquals("0792315847", aColsAccess.getColsSite().getContactTelephone());
		assertEquals("Peter.Schuhmacher@bluewin.ch", aColsAccess.getColsSite().getEmail());

		// Check Dealer values
		assertEquals("Martin Lala", aColsAccess.getDealer().getName());
		assertEquals("033352352", aColsAccess.getDealer().getPhone());
		assertEquals("033352353", aColsAccess.getDealer().getFax());
		assertEquals("Martin.Lala@bluewin.ch", aColsAccess.getDealer().getEmail());

		// Check Cpe Values
		assertEquals("CpeModel-1", aColsAccess.getLogicalPhysicalCpe().getCpe().getCpeModel());
		assertEquals("CpeName-1", aColsAccess.getLogicalPhysicalCpe().getCpe().getCpeName());

		// CPE WAN
		assertEquals("CpeWanPortName-1", aColsAccess.getLogicalPhysicalCpe().getCpe().getCpeWanPortName());
		assertEquals("CpeWanPortPluggable-1", aColsAccess.getLogicalPhysicalCpe().getCpe().getCpeWanPortPluggable());

		// CPE LAN
		Cpe aCpe = aColsAccess.getLogicalPhysicalCpe().getCpe();

		assertAccessMultiplePlug(aCpe);
	}

	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/cpeData.xml" })
	public void canCreate_accessLowEndCopper() throws ObjectNotFoundException {
		canCreate_accessLowEndCopper(false, false);
		checkSendingWfmDoneDate("EOE-0000001");
	}

	private void checkSendingWfmDoneDate(String mainOrder) {
		// check if we sent WfmDoneDate to CFU
		OrderX mainOrderX = EoeOrderUtils.asX(EoeOrderUtils.findOrder(mainOrder, getModifiedSetter()));
		OrderX checkCpeInstallationWfmDone = mainOrderX.getSubOrderX(ColsOrderType.COLS_CPE_INSTALLATION_DONE_ORDER.getType());
		Assert.assertNotNull(checkCpeInstallationWfmDone);
		List<EswLog> eswLogs = ServiceLocatorCdi.cdi(EswLogRepository.class).findByOrderId(checkCpeInstallationWfmDone.getOrderId());
		UpdateOrderStatusRequestType updateStatusRequest = null;
		for (EswLog eswLog : eswLogs) {
			if (eswLog.getMessage().startsWith("Sent Notification XML")) {
				updateStatusRequest = XmlUtils.marshaller().unmarshal(eswLog.getMessageLong(), UpdateOrderStatusRequestType.class);
				break;
			}
		}
		Assert.assertNotNull(updateStatusRequest);
		Assert.assertEquals("WfmDoneDate", updateStatusRequest.getOrderItem().get(0).getAttributes().getAttribute().get(1).getName());
		Assert.assertEquals("2000-01-13 01:02", updateStatusRequest.getOrderItem().get(0).getAttributes().getAttribute().get(1).getValue());
	}

	@Test
	public void canCreate_accessLowEndCopper_NoCpe() throws ObjectNotFoundException {
		canCreate_accessLowEndCopper(true, false);
	}

	/**
	 * Delete LEC without Connectivities
	 */
	@Test
	public void canCreate_N20LowEndCopperDelete() throws Exception {
		canCreate_accessLowEndCopper();

		OrderX n20 = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", getModifiedSetter()));
		OrderX s33PreOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.001", getModifiedSetter()));
		Assert.assertEquals("pre order should be ", OrderState.FINISHED, s33PreOrder.getState());
		Assert.assertEquals("main order should be ", OrderState.FINISHED, n20.getState());

		// find the ColAccess we just created:
		ColsProcessAccessCreateLowEnd proc = n20.getObjectEvaluatorX().getSingleNotNull(ColsProcessAccessCreateLowEnd.class,
				OrderObjectType.DATA);

		// we need this one to delete
		String colsAccessId = proc.getResponse().getColsAccessId();
		Assert.assertNotNull("colsAccessId should be available", colsAccessId);

		// try to delete the ColsAccess

		// Create the Order to hit the NorthBound

		Order deleteOrder = createDeleteColsAccessOrder();

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(deleteOrder);

		// Checking sent attributes to WFM.

		ProcessMsgCPEOrderingOperationRequest wfmReq = this.pollLastRequest(ProcessMsgCPEOrderingOperationRequest.class);

		// Tracking Information
		Assert.assertNotNull(wfmReq.getTrackingInformation());
		Assert.assertEquals("543210", wfmReq.getTrackingInformation().getEventLocalID());
		Assert.assertEquals("OE-COLS", wfmReq.getTrackingInformation().getApplicationID());
		Assert.assertEquals("OH-CIH", wfmReq.getTrackingInformation().getOriginApplicationID());

		// Header
		Assert.assertNotNull(wfmReq.getProcessContentCPEOrderingOperationReq().getHeader());
		Assert.assertNotNull(wfmReq.getProcessContentCPEOrderingOperationReq().getHeader().getTimestamp());
		Assert.assertEquals("INI", wfmReq.getProcessContentCPEOrderingOperationReq().getHeader().getFsFlag());

		Assert.assertEquals("createUpdate", wfmReq.getProcessContentCPEOrderingOperationReq().getHeader().getOperation());

		// It's null in the Creation.
		Assert.assertNull(wfmReq.getProcessContentCPEOrderingOperationReq().getHeader().getWfmId());

		// Order
		Assert.assertEquals("EOE-0000002.002", wfmReq.getProcessContentCPEOrderingOperationReq().getOrder().getEswId());
		Assert.assertEquals(30, wfmReq.getProcessContentCPEOrderingOperationReq().getOrder().getStatus());
		Assert.assertNotNull(wfmReq.getProcessContentCPEOrderingOperationReq().getOrder().getRequestedInstallationEarliestStartDate());
		Assert.assertNotNull(wfmReq.getProcessContentCPEOrderingOperationReq().getOrder().getRequestedInstallationLatestEndDate());

		// It's setting "false" in the Creation.
		Assert.assertEquals(false, wfmReq.getProcessContentCPEOrderingOperationReq().getOrder().isIsFixedAppointment());

		Assert.assertNotNull(wfmReq.getProcessContentCPEOrderingOperationReq().getOrder().getOrderDate());
		Assert.assertEquals("Bern", wfmReq.getProcessContentCPEOrderingOperationReq().getOrder().getCompanyCity());
		Assert.assertEquals("1753", wfmReq.getProcessContentCPEOrderingOperationReq().getOrder().getCompanyZipCode());
		Assert.assertEquals("CpeName=CpeName-1", wfmReq.getProcessContentCPEOrderingOperationReq().getOrder().getRemarks());

		// Get the order
		OrderX order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002", this.getModifiedSetter()));

		OrderX checkIfConnectivitiesOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002.001", getModifiedSetter()));
		OrderX sendDeinstallationOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002.002", getModifiedSetter()));
		OrderX deinstallationDoneOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002.003", getModifiedSetter()));

		OrderX s33DeleteOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002.004", getModifiedSetter()));
		OrderX lecBasicConnecOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002.005", getModifiedSetter()));
		OrderX lowEndOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002.006", getModifiedSetter()));
		OrderX s35DeleteOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002.007", getModifiedSetter()));

		Assert.assertNotNull("delete order not found", order);

		OrderDetails od = order.process();
		Assert.assertFalse(od.isHasErrors());

		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());

		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrder = nbOrderService.findByOrderId("EOE-0000002");
		Assert.assertNotNull(nbOrder);
		Assert.assertEquals(7, nbOrder.getSuborders().size());

		ColsProcessAccessDelete process = (ColsProcessAccessDelete) nbOrder.getColsProcess();

		// Simulate WFM Response
		int wfmId = Integer.valueOf(process.getResponse().getWfmId());
		ServiceLocatorCdi.cdi(ColsWfmSimulator.class).simulateResponse(wfmId, ColsWfmSimulator.CLOSED);

		// Get the ColsResponse from the WFM notification
		ColsResponse wfmColsResponse = this.getRequestHolder().pollRequest(ColsResponse.class);
		Boolean wfmEnabled = AttributeManagerUtil.getSystemPropertyBoolean("cols.wfm.ennabled");
		if (wfmEnabled) {
			Assert.assertTrue(wfmColsResponse.isSuccessful());
		}

		// Make sure order is not finished yet
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, checkIfConnectivitiesOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, sendDeinstallationOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, deinstallationDoneOrder.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, s33DeleteOrder.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, lecBasicConnecOrder.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, lowEndOrder.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, s35DeleteOrder.getState());

		// Check sent order
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_n20_LowEnd_delete_copeOrder.xml");

		// Make sure order is now not yet finished (waiting for CoPE)
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());

		// Simulate CoPE Notification
		new LcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		// Check notification XML
		this.assertLastNotificationXml("ColsServiceIT_n20Delete_notification.xml");

		// Make sure order is now finished
		Assert.assertEquals(OrderState.FINISHED, order.getState());
		Assert.assertEquals(OrderState.FINISHED, s33DeleteOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, lecBasicConnecOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, lowEndOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, s35DeleteOrder.getState());

		// Check information in System Interaction
		OrderX mainOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", getModifiedSetter()));
		Assert.assertNotNull(mainOrder);

		SystemInteractionEntryService anInteractionService = ServiceLocatorCdi.cdi(SystemInteractionEntryService.class);
		List<ISystemInteractionEntry> interactionList = anInteractionService.findByOrderId(mainOrder.getOrderId());
		assertEquals(1, interactionList.size());

		// Check deletion attributes
		ColsAccessRepository colsAccessRepository = ServiceLocatorCdi.cdi(ColsAccessRepository.class, QualifierUtils.COLS_ANNO);
		List<ColsAccess> colsAccessesList = colsAccessRepository.findAll();
		assertEquals(1, colsAccessesList.size());
		assertNotNull(colsAccessesList.get(0).getDeleteDate());
		assertEquals("OH-CIH", colsAccessesList.get(0).getOrderCreator());
	}

	/**
	 * Delete LEC without Connectivities
	 */
	@Test
	public void canCreate_N20LowEndCopperDeleteMultipleRequests() throws Exception {
		canCreate_accessLowEndCopper();

		OrderX n20 = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", getModifiedSetter()));
		OrderX s33PreOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.001", getModifiedSetter()));
		Assert.assertEquals("pre order should be ", OrderState.FINISHED, s33PreOrder.getState());
		Assert.assertEquals("main order should be ", OrderState.FINISHED, n20.getState());

		// find the ColAccess we just created:
		ColsProcessAccessCreateLowEnd proc = n20.getObjectEvaluatorX().getSingleNotNull(ColsProcessAccessCreateLowEnd.class,
				OrderObjectType.DATA);

		// we need this one to delete
		String colsAccessId = proc.getResponse().getColsAccessId();
		Assert.assertNotNull("colsAccessId should be available", colsAccessId);

		// try to delete the ColsAccess

		// Create the Order to hit the NorthBound

		Order deleteOrder = createDeleteColsAccessOrder();

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(deleteOrder);

		// Checking sent attributes to WFM.

		ProcessMsgCPEOrderingOperationRequest wfmReq = this.pollLastRequest(ProcessMsgCPEOrderingOperationRequest.class);

		// Tracking Information
		Assert.assertNotNull(wfmReq.getTrackingInformation());
		Assert.assertEquals("543210", wfmReq.getTrackingInformation().getEventLocalID());
		Assert.assertEquals("OE-COLS", wfmReq.getTrackingInformation().getApplicationID());
		Assert.assertEquals("OH-CIH", wfmReq.getTrackingInformation().getOriginApplicationID());

		// Header
		Assert.assertNotNull(wfmReq.getProcessContentCPEOrderingOperationReq().getHeader());
		Assert.assertNotNull(wfmReq.getProcessContentCPEOrderingOperationReq().getHeader().getTimestamp());
		Assert.assertEquals("INI", wfmReq.getProcessContentCPEOrderingOperationReq().getHeader().getFsFlag());

		Assert.assertEquals("createUpdate", wfmReq.getProcessContentCPEOrderingOperationReq().getHeader().getOperation());

		// It's null in the Creation.
		Assert.assertNull(wfmReq.getProcessContentCPEOrderingOperationReq().getHeader().getWfmId());

		// Order
		Assert.assertEquals("EOE-0000002.002", wfmReq.getProcessContentCPEOrderingOperationReq().getOrder().getEswId());
		Assert.assertEquals(30, wfmReq.getProcessContentCPEOrderingOperationReq().getOrder().getStatus());
		Assert.assertNotNull(wfmReq.getProcessContentCPEOrderingOperationReq().getOrder().getRequestedInstallationEarliestStartDate());
		Assert.assertNotNull(wfmReq.getProcessContentCPEOrderingOperationReq().getOrder().getRequestedInstallationLatestEndDate());

		// It's setting "false" in the Creation.
		Assert.assertEquals(false, wfmReq.getProcessContentCPEOrderingOperationReq().getOrder().isIsFixedAppointment());

		Assert.assertNotNull(wfmReq.getProcessContentCPEOrderingOperationReq().getOrder().getOrderDate());
		Assert.assertEquals("Bern", wfmReq.getProcessContentCPEOrderingOperationReq().getOrder().getCompanyCity());
		Assert.assertEquals("1753", wfmReq.getProcessContentCPEOrderingOperationReq().getOrder().getCompanyZipCode());
		Assert.assertEquals("CpeName=CpeName-1", wfmReq.getProcessContentCPEOrderingOperationReq().getOrder().getRemarks());

		// Get the order
		OrderX order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002", this.getModifiedSetter()));

		OrderX checkIfConnectivitiesOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002.001", getModifiedSetter()));
		OrderX sendDeinstallationOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002.002", getModifiedSetter()));
		OrderX deinstallationDoneOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002.003", getModifiedSetter()));

		OrderX s33DeleteOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002.004", getModifiedSetter()));
		OrderX lecBasicConnecOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002.005", getModifiedSetter()));
		OrderX lowEndOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002.006", getModifiedSetter()));
		OrderX s35DeleteOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002.007", getModifiedSetter()));

		Assert.assertNotNull("delete order not found", order);

		OrderDetails od = order.process();
		Assert.assertFalse(od.isHasErrors());

		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());

		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrder = nbOrderService.findByOrderId("EOE-0000002");
		Assert.assertNotNull(nbOrder);
		Assert.assertEquals(7, nbOrder.getSuborders().size());

		ColsProcessAccessDelete process = (ColsProcessAccessDelete) nbOrder.getColsProcess();

		// Simulate WFM Response
		int wfmId = Integer.valueOf(process.getResponse().getWfmId());
		ServiceLocatorCdi.cdi(ColsWfmSimulator.class).simulateResponse(wfmId, ColsWfmSimulator.CLOSED);

		// Get the ColsResponse from the WFM notification
		ColsResponse wfmColsResponse = this.getRequestHolder().pollRequest(ColsResponse.class);
		Boolean wfmEnabled = AttributeManagerUtil.getSystemPropertyBoolean("cols.wfm.ennabled");
		if (wfmEnabled) {
			Assert.assertTrue(wfmColsResponse.isSuccessful());
		}

		// Make sure order is not finished yet
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, checkIfConnectivitiesOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, sendDeinstallationOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, deinstallationDoneOrder.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, s33DeleteOrder.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, lecBasicConnecOrder.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, lowEndOrder.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, s35DeleteOrder.getState());

		// Check sent order
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_n20_LowEnd_delete_copeOrder.xml");

		// Make sure order is now not yet finished (waiting for CoPE)
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());

		// Create the Order to hit the NorthBound
		Order deleteOrder2 = createDeleteColsAccessOrder();

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(deleteOrder2);

		// Get the second order
		OrderX order2 = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003", this.getModifiedSetter()));

		Assert.assertEquals(OrderState.REJECTED, order2.getState());
	}

	/**
	 * Delete LEC with Connectivities
	 */
	@Test
	public void canDeleteLowEndWithConnectivities() throws Exception {

		// create a connectivity -> which would then obviously need an L2Net and a ColsAccess
		ColsAccess colsAccess = this.testData().cols().newColsAccess("1", ColsAccessLegType.LOW_END);
		colsAccess.setColsAccessId("ALL:SUB:1");
		ServiceLocatorCdi.cdi(ColsAccessService.class).update(colsAccess);

		L2Net l2Net = this.testData().cols().newL2Net("1", "CES");
		L2ServiceConnectivity connectivity = this.testData().cols().newL2ServiceConnectivity("1", colsAccess, l2Net);

		Assert.assertNotNull(colsAccess);
		Assert.assertNotNull(l2Net);
		Assert.assertNotNull(connectivity);

		// try to delete the ColsAccess

		// Create the Order to hit the NorthBound

		Order deleteOrder = createDeleteColsAccessOrder();

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(deleteOrder);
	}

	@Test
	public void canCreate_accessLowEndCopper_with_3Plugs_GUI_DELETES_ALL_CPELANS() {
		OrderX preOrder = this.submitAccessLowEndCopper(false); // 3 pluggables set in ColsAccess

		EoeOrderServiceCols eoeOrderServiceCols = ServiceLocatorCdi.cdi(EoeOrderServiceCols.class);

		// Get SubOrders we will make checks on

		OrderX accessLowEndPreOrderTibcoOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ACCESS_LOW_END_PRE_ORDER.getType());

		// Process the order to make sure it's processed without errors
		OrderDetails od = preOrder.process();
		Assert.assertFalse(od.isHasErrors());

		// Suborder ColsQualyByAddress should be in status IN_PROGRESS, waiting for the COLS GUI to assign the UP Data
		OrderX colsQualyByAddressOrder = preOrder.getSubOrderX(ColsOrderType.COLS_QUALI_BY_ADDRESS.getType());
		Assert.assertEquals(OrderState.IN_PROGRESS, colsQualyByAddressOrder.getState());

		// Call EoeOrderServiceCols service to set UP data

		UpData upData = new UpData();
		upData.setAccessNet("accessNet");
		upData.setMaxAccessSpeedDown("100M"); // Format?
		upData.setMaxAccessSpeedUp("100M"); // Format?
		upData.setSse("1");
		upData.setTaxRegion("2");
		upData.setUnitNumber("3");
		upData.setUnitType("4");
		upData.setUpPreparation("upPrep");

		// Make sure we can get an UpData list
		ColsSite site = eoeOrderServiceCols.getSite(colsQualyByAddressOrder.getOrderId(), this.getMetaData());
		// Make sure we can get an UpData list
		List<UpData> upDatas = eoeOrderServiceCols.getUpDataList(
				colsQualyByAddressOrder.getOrderId(), this.getMetaData(),
				site.getStreet(), site.getHouseNumber(),
				OrderDetailPm.convertPostalCode(site.getPostalCode()), site.getCity());

		Assert.assertTrue(upDatas.size() > 0);
		eoeOrderServiceCols.saveUpData(colsQualyByAddressOrder.getOrderId(), upData, this.getMetaData());

		// ColsQualyByAddress should be finished now
		Assert.assertEquals(OrderState.FINISHED, colsQualyByAddressOrder.getState());

		// Suborder ColsQualiByStartPoint should be in status IN_PROGRESS, waiting for the COLS GUI to assign time slog
		OrderX colsQualiByStartPointOrder = preOrder.getSubOrderX(ColsOrderType.COLS_QUALI_BY_START_POINT.getType());
		Assert.assertEquals(OrderState.IN_PROGRESS, colsQualiByStartPointOrder.getState());

		// Call EoeOrderServiceCols service to assign time slot
		ColsTimeSlot timeSlot = new ColsTimeSlot();
		timeSlot.setFulfillmentTimeSlotQualifyIndex(1L);
		timeSlot.setQualificationIndex(2L);
		timeSlot.setQualificationNumber(3L);

		// Make sure we can get a time slot list
		List<ColsTimeSlot> timeSlots = eoeOrderServiceCols.getTimeSlotList(colsQualiByStartPointOrder.getOrderId(), this.getMetaData());
		Assert.assertTrue(timeSlots.size() > 0);
		eoeOrderServiceCols.saveTimeSlot(colsQualiByStartPointOrder.getOrderId(), timeSlot, this.getMetaData(), "testUser");

		// ColsQualyByAddress should be finished now
		Assert.assertEquals(OrderState.FINISHED, colsQualiByStartPointOrder.getState());

		// Simulate the GUI Assign of CPE Data.
		OrderX assignCpeOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE.getType());

		/** SET CPE INFORMATION WITH NO PLUG on the Responses **/
		setCpeDataWithNoPlugs(assignCpeOrder, false);

		// Check the COPE order XML - no plugs should show
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_access_low_end_no_plug_copeOrder.xml");

		// Check Main Order ready notification XML - the first notification should not reflect the update yet, 3 plugs should still show
		this.assertLastNotificationXml("ColsServiceIT_access_low_end_main_ready_notification.xml"); // really expected

		// -----------------------------------------
		// The main order should be in progress
		Assert.assertEquals(OrderState.IN_PROGRESS, preOrder.getState());

		// accessLowEndPreOrderTibcoOrder should be in progress, waiting for manual update of the data
		Assert.assertEquals(OrderState.IN_PROGRESS, accessLowEndPreOrderTibcoOrder.getState());

		// find the ColAccessProcess we just created:
		ColsProcessAccessCreateLowEnd aProcess = preOrder.getDataObjectNotNull(ColsProcessAccessCreateLowEnd.class, true);
		CreateColsAccessLowEndProcessService aService = ServiceLocatorCdi.cdi(CreateColsAccessLowEndProcessService.class);
		try {
			// to ensure that the persisted process is retrieved, get the process once more, this time by Id
			aProcess = aService.searchById(aProcess.getId());
		} catch (Exception ex) {
			throw new ServiceDbRuntimeException("Could not locate ColsProcessAccessCreateLowEnd for process id " + aProcess.getId(), ex);
		}

		// Check CpeLanList on the Request (to make sure its not the same list as on the request)
		// ---------------------------------------------------

		// get the Cpe on the Request from the Process
		Cpe cpe = aProcess.getRequest().getCpe();
		SortedSet<CpeLan> cpeLanList = cpe.getCpeLanList();

		// there should be 3 items in the CpeLanList on the Request
		Assert.assertEquals(3, cpeLanList.size());

		for (CpeLan cpeLan : cpeLanList) {
			Assert.assertNotNull(cpeLan.getCpeLanPortName()); // on the Request
		}

		// Check CpeLanList on the Response ---------------------------------------------------

		// get the Cpe on the Response from the Process - it should have nothing in the CpeLanList
		cpe = aProcess.getResponse().getCpe();
		Assert.assertTrue(cpe.getCpeLanList().isEmpty());
	}

	@Test
	public void canCreate_n20Cancel() {
		this.canCreate_n20Cancel(false);
	}

	@Test
	public void canCreate_n20Cancel_eoeOrderService() {
		this.canCreate_n20Cancel(true);
	}

	@Test
	public void canCreate_accessLowEndCopper_with_3Plugs_GUI_UPDATES_LANPORTNAMES() {
		OrderX preOrder = this.submitAccessLowEndCopper(false);
		EoeOrderServiceCols eoeOrderServiceCols = ServiceLocatorCdi.cdi(EoeOrderServiceCols.class);

		// Get SubOrders we will make checks on

		OrderX accessLowEndPreOrderTibcoOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ACCESS_LOW_END_PRE_ORDER.getType());

		// Process the order to make sure it's processed without errors
		OrderDetails od = preOrder.process();
		Assert.assertFalse(od.isHasErrors());

		// Suborder ColsQualyByAddress should be in status IN_PROGRESS, waiting for the COLS GUI to assign the UP Data
		OrderX colsQualyByAddressOrder = preOrder.getSubOrderX(ColsOrderType.COLS_QUALI_BY_ADDRESS.getType());
		Assert.assertEquals(OrderState.IN_PROGRESS, colsQualyByAddressOrder.getState());

		// Call EoeOrderServiceCols service to set UP data

		UpData upData = new UpData();
		upData.setAccessNet("accessNet");
		upData.setMaxAccessSpeedDown("100M"); // Format?
		upData.setMaxAccessSpeedUp("100M"); // Format?
		upData.setSse("1");
		upData.setTaxRegion("2");
		upData.setUnitNumber("3");
		upData.setUnitType("4");
		upData.setUpPreparation("upPrep");

		// Make sure we can get an UpData list
		ColsSite site = eoeOrderServiceCols.getSite(colsQualyByAddressOrder.getOrderId(), this.getMetaData());
		List<UpData> upDatas = eoeOrderServiceCols.getUpDataList(
				colsQualyByAddressOrder.getOrderId(), this.getMetaData(),
				site.getStreet(), site.getHouseNumber(),
				OrderDetailPm.convertPostalCode(site.getPostalCode()), site.getCity());

		Assert.assertTrue(upDatas.size() > 0);
		eoeOrderServiceCols.saveUpData(colsQualyByAddressOrder.getOrderId(), upData, this.getMetaData());

		// ColsQualyByAddress should be finished now
		Assert.assertEquals(OrderState.FINISHED, colsQualyByAddressOrder.getState());

		// Suborder ColsQualiByStartPoint should be in status IN_PROGRESS, waiting for the COLS GUI to assign time slog
		OrderX colsQualiByStartPointOrder = preOrder.getSubOrderX(ColsOrderType.COLS_QUALI_BY_START_POINT.getType());
		Assert.assertEquals(OrderState.IN_PROGRESS, colsQualiByStartPointOrder.getState());

		// Call EoeOrderServiceCols service to assign time slot
		ColsTimeSlot timeSlot = new ColsTimeSlot();
		timeSlot.setFulfillmentTimeSlotQualifyIndex(1L);
		timeSlot.setQualificationIndex(2L);
		timeSlot.setQualificationNumber(3L);

		// Make sure we can get a time slot list
		List<ColsTimeSlot> timeSlots = eoeOrderServiceCols.getTimeSlotList(colsQualiByStartPointOrder.getOrderId(), this.getMetaData());
		Assert.assertTrue(timeSlots.size() > 0);
		eoeOrderServiceCols.saveTimeSlot(colsQualiByStartPointOrder.getOrderId(), timeSlot, this.getMetaData(), "testUser");

		// ColsQualyByAddress should be finished now
		Assert.assertEquals(OrderState.FINISHED, colsQualiByStartPointOrder.getState());

		// Simulate the GUI Assign of CPE Data. GUI calls same service as below:
		OrderX assignCpeOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE.getType());

		/** SET CPE INFORMATION WITH MULTIPLE PLUG **/
		setCpeDataWithMultiplePlug(assignCpeOrder, false);

		// Check the COPE order XML
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_access_low_end_withMultiplePlug_copeOrder.xml");

		// Check Main Order ready notification XML - the first notification should not reflect the update yet
		this.assertLastNotificationXml("ColsServiceIT_access_low_end_main_ready_notification.xml"); // really expected

		// -----------------------------------------
		// The main order should be in progress
		Assert.assertEquals(OrderState.IN_PROGRESS, preOrder.getState());

		// accessLowEndPreOrderTibcoOrder should be in progress, waiting for manual update of the data
		Assert.assertEquals(OrderState.IN_PROGRESS, accessLowEndPreOrderTibcoOrder.getState());

		// find the ColAccessProcess we just created:
		ColsProcessAccessCreateLowEnd aProcess = preOrder.getDataObjectNotNull(ColsProcessAccessCreateLowEnd.class, true);
		CreateColsAccessLowEndProcessService aService = ServiceLocatorCdi.cdi(CreateColsAccessLowEndProcessService.class);
		try {
			// to ensure that the persisted process is retrieved, get the process once more, this time by Id
			aProcess = aService.searchById(aProcess.getId());
		} catch (Exception ex) {
			throw new ServiceDbRuntimeException("Could not locate ColsProcessAccessCreateLowEnd for process id " + aProcess.getId(), ex);
		}

		// Check CpeLanList on the Request ---------------------------------------------------

		// get the Cpe on the Request from the Process
		Cpe cpe = aProcess.getRequest().getCpe();
		SortedSet<CpeLan> cpeLanList = cpe.getCpeLanList();

		// there should be 3 items in the CpeLanList
		Assert.assertEquals(3, cpeLanList.size());

		for (CpeLan cpeLan : cpeLanList) {
			Assert.assertNotNull(cpeLan.getCpeLanPortName());
		}

		// Check CpeLanList on the Response ---------------------------------------------------

		// get the Cpe on the Response from the Process
		cpe = aProcess.getResponse().getCpe();
		cpeLanList = cpe.getCpeLanList();

		// there should be 3 items in the CpeLanList
		Assert.assertEquals(5, cpeLanList.size());

		for (CpeLan cpeLan : cpeLanList) {
			Assert.assertNotNull(cpeLan.getCpeLanPortName());
		}

		check5Pluggables(cpeLanList);

		// assertEquals(1, ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).compareOrdersCount());
	}

	@Test
	public void canCreate_accessLowEndCopper_with_NoPlugs_GUI_ADDS_3CPELANS() {
		OrderX preOrder = this.submitAccessLowEndCopperNoPlugs();
		EoeOrderServiceCols eoeOrderServiceCols = ServiceLocatorCdi.cdi(EoeOrderServiceCols.class);

		// Get SubOrders we will make checks on

		OrderX accessLowEndPreOrderTibcoOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ACCESS_LOW_END_PRE_ORDER.getType());

		// Process the order to make sure it's processed without errors
		OrderDetails od = preOrder.process();
		Assert.assertFalse(od.isHasErrors());

		// Suborder ColsQualyByAddress should be in status IN_PROGRESS, waiting for the COLS GUI to assign the UP Data
		OrderX colsQualyByAddressOrder = preOrder.getSubOrderX(ColsOrderType.COLS_QUALI_BY_ADDRESS.getType());
		Assert.assertEquals(OrderState.IN_PROGRESS, colsQualyByAddressOrder.getState());

		// Call EoeOrderServiceCols service to set UP data

		UpData upData = new UpData();
		upData.setAccessNet("accessNet");
		upData.setMaxAccessSpeedDown("100M"); // Format?
		upData.setMaxAccessSpeedUp("100M"); // Format?
		upData.setSse("1");
		upData.setTaxRegion("2");
		upData.setUnitNumber("3");
		upData.setUnitType("4");
		upData.setUpPreparation("upPrep");

		// Make sure we can get an UpData list
		ColsSite site = eoeOrderServiceCols.getSite(colsQualyByAddressOrder.getOrderId(), this.getMetaData());
		List<UpData> upDatas = eoeOrderServiceCols.getUpDataList(
				colsQualyByAddressOrder.getOrderId(), this.getMetaData(),
				site.getStreet(), site.getHouseNumber(),
				OrderDetailPm.convertPostalCode(site.getPostalCode()), site.getCity());

		Assert.assertTrue(upDatas.size() > 0);
		eoeOrderServiceCols.saveUpData(colsQualyByAddressOrder.getOrderId(), upData, this.getMetaData());

		// ColsQualyByAddress should be finished now
		Assert.assertEquals(OrderState.FINISHED, colsQualyByAddressOrder.getState());

		// Suborder ColsQualiByStartPoint should be in status IN_PROGRESS, waiting for the COLS GUI to assign time slog
		OrderX colsQualiByStartPointOrder = preOrder.getSubOrderX(ColsOrderType.COLS_QUALI_BY_START_POINT.getType());
		Assert.assertEquals(OrderState.IN_PROGRESS, colsQualiByStartPointOrder.getState());

		// Call EoeOrderServiceCols service to assign time slot
		ColsTimeSlot timeSlot = new ColsTimeSlot();
		timeSlot.setFulfillmentTimeSlotQualifyIndex(1L);
		timeSlot.setQualificationIndex(2L);
		timeSlot.setQualificationNumber(3L);

		// Make sure we can get a time slot list
		List<ColsTimeSlot> timeSlots = eoeOrderServiceCols.getTimeSlotList(colsQualiByStartPointOrder.getOrderId(), this.getMetaData());
		Assert.assertTrue(timeSlots.size() > 0);
		eoeOrderServiceCols.saveTimeSlot(colsQualiByStartPointOrder.getOrderId(), timeSlot, this.getMetaData(), "testUser");

		// ColsQualyByAddress should be finished now
		Assert.assertEquals(OrderState.FINISHED, colsQualiByStartPointOrder.getState());

		// Simulate the GUI Assign of CPE Data. GUI calls same service as below:
		OrderX assignCpeOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE.getType());

		/** SET CPE INFORMATION WITH MULTIPLE PLUG **/
		setCpeDataWithMultiplePlug(assignCpeOrder, false);

		// Check the COPE order XML - 3 plugs should show
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_access_low_end_withMultiplePlug_copeOrder.xml");

		// Check Main Order ready notification XML - the first notification should not reflect the update yet, no plugs expected
		this.assertLastNotificationXml("ColsServiceIT_access_low_end_main_ready_notification_no_plugs.xml"); // really expected

		// -----------------------------------------
		// The main order should be in progress
		Assert.assertEquals(OrderState.IN_PROGRESS, preOrder.getState());

		// accessLowEndPreOrderTibcoOrder should be in progress, waiting for manual update of the data
		Assert.assertEquals(OrderState.IN_PROGRESS, accessLowEndPreOrderTibcoOrder.getState());

		// find the ColAccessProcess we just created:
		ColsProcessAccessCreateLowEnd aProcess = preOrder.getDataObjectNotNull(ColsProcessAccessCreateLowEnd.class, true);
		CreateColsAccessLowEndProcessService aService = ServiceLocatorCdi.cdi(CreateColsAccessLowEndProcessService.class);
		try {
			// to ensure that the persisted process is retrieved, get the process once more, this time by Id
			aProcess = aService.searchById(aProcess.getId());
		} catch (Exception ex) {
			throw new ServiceDbRuntimeException("Could not locate ColsProcessAccessCreateLowEnd for process id " + aProcess.getId(), ex);
		}

		// Check that Cpe Is Null on the Request ----------------------------------------------

		Assert.assertNotNull(aProcess.getRequest().getCpe());

		// Check CpeLanList on the Response ---------------------------------------------------

		// get the Cpe on the Response from the Process
		Cpe cpe = aProcess.getResponse().getCpe();
		SortedSet<CpeLan> cpeLanList = cpe.getCpeLanList();

		// there should be 3 items in the CpeLanList
		Assert.assertEquals(5, cpeLanList.size());

		for (CpeLan cpeLan : cpeLanList) {
			Assert.assertNotNull(cpeLan.getCpeLanPortName());
		}

		check5Pluggables(cpeLanList);

		// assertEquals(1, ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).compareOrdersCount());
	}

	@Test
	public void canCreate_accessLowEndCopper_with_4Plugs_GUI_DELETES_1CPELAN() {
		OrderX preOrder = this.submitAccessLowEndCopper4Plugs();
		EoeOrderServiceCols eoeOrderServiceCols = ServiceLocatorCdi.cdi(EoeOrderServiceCols.class);

		// Get SubOrders we will make checks on

		OrderX accessLowEndPreOrderTibcoOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ACCESS_LOW_END_PRE_ORDER.getType());

		// Process the order to make sure it's processed without errors
		OrderDetails od = preOrder.process();
		Assert.assertFalse(od.isHasErrors());

		// Suborder ColsQualyByAddress should be in status IN_PROGRESS, waiting for the COLS GUI to assign the UP Data
		OrderX colsQualyByAddressOrder = preOrder.getSubOrderX(ColsOrderType.COLS_QUALI_BY_ADDRESS.getType());
		Assert.assertEquals(OrderState.IN_PROGRESS, colsQualyByAddressOrder.getState());

		// Call EoeOrderServiceCols service to set UP data

		UpData upData = new UpData();
		upData.setAccessNet("accessNet");
		upData.setMaxAccessSpeedDown("100M"); // Format?
		upData.setMaxAccessSpeedUp("100M"); // Format?
		upData.setSse("1");
		upData.setTaxRegion("2");
		upData.setUnitNumber("3");
		upData.setUnitType("4");
		upData.setUpPreparation("upPrep");

		// Make sure we can get an UpData list
		ColsSite site = eoeOrderServiceCols.getSite(colsQualyByAddressOrder.getOrderId(), this.getMetaData());
		List<UpData> upDatas = eoeOrderServiceCols.getUpDataList(
				colsQualyByAddressOrder.getOrderId(), this.getMetaData(),
				site.getStreet(), site.getHouseNumber(),
				OrderDetailPm.convertPostalCode(site.getPostalCode()), site.getCity());

		Assert.assertTrue(upDatas.size() > 0);
		eoeOrderServiceCols.saveUpData(colsQualyByAddressOrder.getOrderId(), upData, this.getMetaData());

		// ColsQualyByAddress should be finished now
		Assert.assertEquals(OrderState.FINISHED, colsQualyByAddressOrder.getState());

		// Suborder ColsQualiByStartPoint should be in status IN_PROGRESS, waiting for the COLS GUI to assign time slog
		OrderX colsQualiByStartPointOrder = preOrder.getSubOrderX(ColsOrderType.COLS_QUALI_BY_START_POINT.getType());
		Assert.assertEquals(OrderState.IN_PROGRESS, colsQualiByStartPointOrder.getState());

		// Call EoeOrderServiceCols service to assign time slot
		ColsTimeSlot timeSlot = new ColsTimeSlot();
		timeSlot.setFulfillmentTimeSlotQualifyIndex(1L);
		timeSlot.setQualificationIndex(2L);
		timeSlot.setQualificationNumber(3L);

		// Make sure we can get a time slot list
		List<ColsTimeSlot> timeSlots = eoeOrderServiceCols.getTimeSlotList(colsQualiByStartPointOrder.getOrderId(), this.getMetaData());
		Assert.assertTrue(timeSlots.size() > 0);
		eoeOrderServiceCols.saveTimeSlot(colsQualiByStartPointOrder.getOrderId(), timeSlot, this.getMetaData(), "testUser");

		// ColsQualyByAddress should be finished now
		Assert.assertEquals(OrderState.FINISHED, colsQualiByStartPointOrder.getState());

		// Simulate the GUI Assign of CPE Data. GUI calls same service as below:
		OrderX assignCpeOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE.getType());

		/** SET CPE INFORMATION WITH MULTIPLE PLUG **/
		setCpeDataWithMultiplePlug(assignCpeOrder, false);

		// Check the COPE order XML - 3 plugs should show
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_access_low_end_withMultiplePlug_copeOrder.xml");

		// Check Main Order ready notification XML - the first notification should not reflect the update yet
		this.assertLastNotificationXml("ColsServiceIT_access_low_end_main_ready_notification_4plug.xml"); // really expected

		// -----------------------------------------
		// The main order should be in progress
		Assert.assertEquals(OrderState.IN_PROGRESS, preOrder.getState());

		// accessLowEndPreOrderTibcoOrder should be in progress, waiting for manual update of the data
		Assert.assertEquals(OrderState.IN_PROGRESS, accessLowEndPreOrderTibcoOrder.getState());

		// find the ColAccessProcess we just created:
		ColsProcessAccessCreateLowEnd aProcess = preOrder.getDataObjectNotNull(ColsProcessAccessCreateLowEnd.class, true);
		CreateColsAccessLowEndProcessService aService = ServiceLocatorCdi.cdi(CreateColsAccessLowEndProcessService.class);
		try {
			// to ensure that the persisted process is retrieved, get the process once more, this time by Id
			aProcess = aService.searchById(aProcess.getId());
		} catch (Exception ex) {
			throw new ServiceDbRuntimeException("Could not locate CreateColsAccessLowEndProcess for process id " + aProcess.getId(), ex);
		}

		// Check CpeLanList on the Request ---------------------------------------------------

		// get the Cpe on the Request from the Process
		Cpe cpe = aProcess.getRequest().getCpe();
		SortedSet<CpeLan> cpeLanList = cpe.getCpeLanList();

		// there should be 4 items in the CpeLanList
		Assert.assertEquals(4, cpeLanList.size());

		for (CpeLan cpeLan : cpeLanList) {
			Assert.assertNotNull(cpeLan.getCpeLanPortName());
		}

		// Check CpeLanList on the Response ---------------------------------------------------

		// get the Cpe on the Response from the Process
		cpe = aProcess.getResponse().getCpe();
		cpeLanList = cpe.getCpeLanList();

		// there should be 3 items in the CpeLanList
		Assert.assertEquals(5, cpeLanList.size());

		for (CpeLan cpeLan : cpeLanList) {
			Assert.assertNotNull(cpeLan.getCpeLanPortName());
		}

		check5Pluggables(cpeLanList);

		// assertEquals(1, ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).compareOrdersCount());
	}

	/**
	 * Test case for a new Access Low End Copper with Multiple Plug.
	 * 
	 * @throws ObjectNotFoundException
	 */
	@Test
	public void canCreate_accessLowEndCopper_withMultiplePlug() throws ObjectNotFoundException {
		OrderX preOrder = this.submitAccessLowEndCopper(false);
		EoeOrderServiceCols eoeOrderServiceCols = ServiceLocatorCdi.cdi(EoeOrderServiceCols.class);

		// Get SubOrders we will make checks on

		OrderX accessLowEndPreOrderTibcoOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ACCESS_LOW_END_PRE_ORDER.getType());

		// Process the order to make sure it's processed without errors
		OrderDetails od = preOrder.process();
		Assert.assertFalse(od.isHasErrors());

		// Suborder ColsQualyByAddress should be in status IN_PROGRESS, waiting for the COLS GUI to assign the UP Data
		OrderX colsQualyByAddressOrder = preOrder.getSubOrderX(ColsOrderType.COLS_QUALI_BY_ADDRESS.getType());
		Assert.assertEquals(OrderState.IN_PROGRESS, colsQualyByAddressOrder.getState());

		// Call EoeOrderServiceCols service to set UP data

		UpData upData = new UpData();
		upData.setAccessNet("accessNet");
		upData.setMaxAccessSpeedDown("100M"); // Format?
		upData.setMaxAccessSpeedUp("100M"); // Format?
		upData.setSse("1");
		upData.setTaxRegion("2");
		upData.setUnitNumber("3");
		upData.setUnitType("4");
		upData.setUpPreparation("upPrep");

		// Make sure we can get an UpData list
		ColsSite site = eoeOrderServiceCols.getSite(colsQualyByAddressOrder.getOrderId(), this.getMetaData());
		List<UpData> upDatas = eoeOrderServiceCols.getUpDataList(
				colsQualyByAddressOrder.getOrderId(), this.getMetaData(),
				site.getStreet(), site.getHouseNumber(),
				OrderDetailPm.convertPostalCode(site.getPostalCode()), site.getCity());

		Assert.assertTrue(upDatas.size() > 0);
		eoeOrderServiceCols.saveUpData(colsQualyByAddressOrder.getOrderId(), upData, this.getMetaData());

		// ColsQualyByAddress should be finished now
		Assert.assertEquals(OrderState.FINISHED, colsQualyByAddressOrder.getState());

		// Suborder ColsQualiByStartPoint should be in status IN_PROGRESS, waiting for the COLS GUI to assign time slog
		OrderX colsQualiByStartPointOrder = preOrder.getSubOrderX(ColsOrderType.COLS_QUALI_BY_START_POINT.getType());
		Assert.assertEquals(OrderState.IN_PROGRESS, colsQualiByStartPointOrder.getState());

		// Call EoeOrderServiceCols service to assign time slot
		ColsTimeSlot timeSlot = new ColsTimeSlot();
		timeSlot.setFulfillmentTimeSlotQualifyIndex(1L);
		timeSlot.setQualificationIndex(2L);
		timeSlot.setQualificationNumber(3L);

		// Make sure we can get a time slot list
		List<ColsTimeSlot> timeSlots = eoeOrderServiceCols.getTimeSlotList(colsQualiByStartPointOrder.getOrderId(), this.getMetaData());
		Assert.assertTrue(timeSlots.size() > 0);
		eoeOrderServiceCols.saveTimeSlot(colsQualiByStartPointOrder.getOrderId(), timeSlot, this.getMetaData(), "testUser");

		// ColsQualyByAddress should be finished now
		Assert.assertEquals(OrderState.FINISHED, colsQualiByStartPointOrder.getState());

		// Simulate the GUI Assign of CPE Data. GUI calls same service as below:
		OrderX assignCpeOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE.getType());

		/** SET CPE INFORMATION WITH MULTIPLE PLUG **/
		setCpeDataWithMultiplePlug(assignCpeOrder, false);

		// Check the COPE order XML - 3 plugs should show here
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_access_low_end_withMultiplePlug_copeOrder.xml");

		// Check Main Order ready notification XML - 3 plugs should show here
		this.assertLastNotificationXml("ColsServiceIT_access_low_end_main_ready_notification.xml"); // really expected

		// The main order should be in progress
		Assert.assertEquals(OrderState.IN_PROGRESS, preOrder.getState());

		// Access Low End Preorder should be in progress, waiting for manual update of the data

		Assert.assertEquals(OrderState.IN_PROGRESS, accessLowEndPreOrderTibcoOrder.getState());

		// Phase Id should already be in progress
		OrderHeaderDataNG ohd = preOrder.getDataObjectNotNull(ColsProcessAccessCreateLowEnd.class, true).getRequest()
				.getOrderHeaderData();
		Assert.assertEquals(Constants.COLS_ORDER_PHASE_PREORDER_IN_PROGRESS, ohd.getPhaseId());

		// Simulate notification from SouthBound
		OrderType submittedOrder = this.pollLastRequest(OrderType.class);
		new LcsNotificationHandler().simulateNotification(submittedOrder);

		// WFM response will be simulated automatically

		// Check Pre Order ready notification XML
		this.assertLastNotificationXml("ColsServiceIT_Lec_access_preorder_withMultiplePlug_ready_notification_setup.xml");

		// Access Low End PreOrder should be confirmed since there is no confirmation order
		Assert.assertEquals(Constants.COLS_ORDER_PHASE_PREORDER_CONFIRMED, ohd.getPhaseId());

		ColsAccessRepository colsAccessRepository = ServiceLocatorCdi.cdi(ColsAccessRepository.class, QualifierUtils.COLS_ANNO);
		// There should be no access created since this is the preordering
		assertEquals(1, colsAccessRepository.count());

		// Make sure the preOrderConfirmed order is still in progress and the phase is confirmed
		Assert.assertEquals(Constants.COLS_ORDER_PHASE_PREORDER_CONFIRMED, ohd.getPhaseId());

		Assert.assertEquals("main order be FINISHED", OrderState.FINISHED, accessLowEndPreOrderTibcoOrder.getState());
		// Main Order should be in progress
		Assert.assertEquals("main order be IN PROGRESS", OrderState.IN_PROGRESS, preOrder.getState());

		// Simulate the GUI Assign of CPE Material. GUI calls same service as below:
		OrderX assignCpeMaterialOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE_MATERIAL.getType());
		/** SET CPE MATERIALS **/
		setCpeMaterial(assignCpeMaterialOrder);

		setCreateColsAccessOrderTasks(preOrder, getMetaData());

		// Check create ColsNorthboundOrder.
		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrder = nbOrderService.findByOrderId("EOE-0000001");
		ColsProcessAccessCreateLowEnd process = (ColsProcessAccessCreateLowEnd) nbOrder.getColsProcess();

		// Simulate WFM Response
		int wfmId = Integer.valueOf(process.getResponse().getWfmId());
		ServiceLocatorCdi.cdi(ColsWfmSimulator.class).simulateResponse(wfmId, ColsWfmSimulator.CLOSED);

		Assert.assertEquals("confirm order should be finished", OrderState.FINISHED, accessLowEndPreOrderTibcoOrder.getState());

		// Activate Alarm
		ServiceLocatorCdi.cdi(ColsOrderAlarmActivationService.class)
				.activateAlarm(accessLowEndPreOrderTibcoOrder.getParentX().getOrderId());

		Assert.assertEquals("main order should be ", OrderState.FINISHED, accessLowEndPreOrderTibcoOrder.getParentX().getState());
		Assert.assertEquals("main order should be ", OrderState.FINISHED, preOrder.getState());

		this.assertLastNotificationXml("ColsServiceIT_access_low_end_preorder_withMultiplePlug_ready_notification.xml");

		// There should be one access created due to the confirmation
		assertEquals(1, colsAccessRepository.count());

		ColsAccessService colsAccessService = ServiceLocatorCdi.cdi(ColsAccessService.class);
		List<ColsAccess> accesses = colsAccessService.searchAll();
		assertEquals(1, accesses.size());
		ColsAccess aColsAccess = accesses.get(0);
		Cpe aCpe = aColsAccess.getLogicalPhysicalCpe().getCpe();

		assertLowEndAccessAttributeValues(aColsAccess, false);

		assertAccessMultiplePlug(aCpe);
	}

	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/cpeData.xml", "testInputData/testUser.xml" })
	public void canCreate_n20() throws ObjectNotFoundException {
		Order newColsOrder = OrderCreation.createNewAccessOrderWithOrderCreator();

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(newColsOrder);

		// Get the order that was implicitly created by the ColsService
		OrderX preOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", this.getModifiedSetter()));

		// Get SubOrders we will make checks on
		OrderX s33PreOrderTibcoOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_S33_PRE_ORDER.getType());
		OrderX n20PreOrderConfirmedOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_N20_PRE_ORDER_CONFIRMED.getType());

		// Process the order to make sure it's processed without errors
		OrderDetails od = preOrder.process();
		Assert.assertFalse(od.isHasErrors());

		// Check create ColsNorthboundOrder.
		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrderPreOrder = nbOrderService.findByOrderId("EOE-0000001");
		Assert.assertEquals(5, nbOrderPreOrder.getSuborders().size());
		ColsProcessAccessCreateHighEnd processCreate = (ColsProcessAccessCreateHighEnd) nbOrderPreOrder.getColsProcess();

		// Make sure the S33 suborder is waiting (for the CPE Model assignment)
		Assert.assertEquals(OrderState.WAITING, s33PreOrderTibcoOrder.getState());

		// The SubOrder COLS_ORDER_ASSIGN_CPE should be in state READY
		OrderX assignCpeOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE.getType());
		Assert.assertEquals(OrderState.IN_PROGRESS, assignCpeOrder.getState());

		// Simulate the GUI Assign of CPE Data. GUI calls same service as below:
		setCpeDataWithOnePlug(preOrder, false);

		Assert.assertEquals(OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getState());

		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_n20_copeOrder_1plug.xml");

		// Check Main Order ready notification XML
		// below should be changed to a notification for only one CpeLan - currently it has 3, like original NB order
		this.assertLastNotificationXml("ColsServiceIT_n20_main_ready_notification.xml");

		// The main order should be in progress
		Assert.assertEquals(OrderState.IN_PROGRESS, preOrder.getState());

		// S33 Preorder should be in progress, waiting for manual update of the data
		Assert.assertEquals(OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getState());

		// ConfirmPreOrder should be waiting as we don't have the necessary data to confirm/cancel yet
		Assert.assertEquals(OrderState.WAITING, n20PreOrderConfirmedOrder.getState());
		// Phase Id should already be in progress
		OrderHeaderDataNG ohd = preOrder.getDataObjectNotNull(ColsProcessAccessCreateHighEnd.class, true).getRequest()
				.getOrderHeaderData();
		Assert.assertEquals(Constants.COLS_ORDER_PHASE_PREORDER_IN_PROGRESS, ohd.getPhaseId());

		// Simulate notification from SouthBound
		OrderType submittedOrder = this.pollLastRequest(OrderType.class);

		// First, CoPE will send an intermediate notification of type SubmitOrderAck with status succeeded,
		// containing one OrderLineItem result for the PO_MSC_HIGH_END_LEG with status succeeded, containing the attribute
		// circuitId.
		// As soon as the circuitId is known, we need to set it on the responses object and send a notification to CFU.

		new LcsNotificationHandler().simulateNotification(submittedOrder, NotificationType.SUBMIT_ORDER_ACK);

		// Make sure the circuitId was stored to the responses object
		ColsResponseAccessCreateHighEnd response = preOrder.getDataObjectNotNull(
				ColsProcessAccessCreateHighEnd.class, true)
				.getResponse();
		Assert.assertNotNull(response.getCircuitId());

		new LcsNotificationHandler().simulateNotification(submittedOrder);

		// Check Pre Order ready notification XML
		this.assertLastNotificationXml("ColsServiceIT_n20_preorder_ready_notification.xml");

		// S33 PreOrder should be in progress now, phase still in progress
		Assert.assertEquals(OrderState.IN_PROGRESS, n20PreOrderConfirmedOrder.getState());
		Assert.assertEquals(Constants.COLS_ORDER_PHASE_PREORDER_IN_PROGRESS, ohd.getPhaseId());

		ColsAccessRepository colsAccessRepository = ServiceLocatorCdi.cdi(ColsAccessRepository.class, QualifierUtils.COLS_ANNO);
		// There should be no access created since this is the preordering
		assertEquals(0, colsAccessRepository.count());

		// Check cpe response values
		response = preOrder.getDataObjectNotNull(ColsProcessAccessCreateHighEnd.class, true)
				.getResponse();
		Assert.assertNotNull(response.getCpe().getCpeModel());
		Assert.assertNotNull(response.getCpe().getCpeName());
		Assert.assertNotNull(response.getCpe().getCpeWanPortPluggable());
		Assert.assertNotNull(response.getCpe().getCpeWanPortName());
		Assert.assertNotNull(response.getCpe().getCpeLanPortPluggable());
		Assert.assertNotNull(response.getCpe().getCpeLanPortName());
		Assert.assertTrue(response.getCpeFinalizedStatus());

		// Confirm this order through the EoeOrderServiceCols
		ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).confirmPreOrder(s33PreOrderTibcoOrder.getOrderId(), getMetaData());

		OrderX n20ConfirmOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002", getModifiedSetter()));
		Assert.assertNotNull(n20ConfirmOrder);

		Assert.assertEquals(OrderState.IN_PROGRESS, n20ConfirmOrder.getSubOrderX(ColsOrderType.COLS_CPE_ORDER_DONE.getType()).getState());
		setCpeOrderDone(n20ConfirmOrder);

		Assert.assertEquals(OrderState.FINISHED, n20ConfirmOrder.getSubOrderX(ColsOrderType.COLS_CPE_ORDER_DONE.getType()).getState());

		// Simulate the GUI Assign of CPE Material. GUI calls same service as below:
		OrderX assignCpeMaterialOrder = n20ConfirmOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE_MATERIAL.getType());
		/** SET CPE MATERIALS **/
		setCpeMaterial(assignCpeMaterialOrder);

		setCreateColsAccessOrderTasks(n20ConfirmOrder, getMetaData());

		nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrderConfirm = nbOrderService.findByOrderId("EOE-0000002");
		ColsProcessAccessConfirm processConfirm = (ColsProcessAccessConfirm) nbOrderConfirm.getColsProcess();

		// Simulate WFM Response
		int wfmId = Integer.valueOf(processConfirm.getResponse().getWfmId());
		ServiceLocatorCdi.cdi(ColsWfmSimulator.class).simulateResponse(wfmId, ColsWfmSimulator.CLOSED);

		// Get the ColsResponse from the WFM notification
		ColsResponse wfmColsResponse = this.getRequestHolder().pollRequest(ColsResponse.class);
		Boolean wfmEnabled = AttributeManagerUtil.getSystemPropertyBoolean("cols.wfm.ennabled");
		if (wfmEnabled) {
			Assert.assertTrue(wfmColsResponse.isSuccessful());
		}

		// Make sure the preOrderConfirmed order is still in progress and the phase is confirmed
		Assert.assertEquals(OrderState.IN_PROGRESS, n20PreOrderConfirmedOrder.getState());
		Assert.assertEquals(Constants.COLS_ORDER_PHASE_PREORDER_CONFIRMED, ohd.getPhaseId());

		// WFM response will be simulated automatically

		// Make sure the confirmation produced a notification
		// this.assertLastNotificationXml("ColsServiceIT_n20_preorder_confirmed_notification.xml");

		// check sent order : to fix the xml
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_n20_copePreOrderConfirm.xml");

		// Main Order should stay in IN_PROGRESS
		Assert.assertEquals("main order should still be in IN_PROGRESS", OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getParentX()
				.getState());

		Assert.assertEquals("confirm order should be in process ", OrderState.IN_PROGRESS, n20ConfirmOrder.getState());

		// Simulate CoPE Notification
		new LcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		// check cfu notification wfm done (SO-3383 ESW-1964)
		UpdateOrderStatusRequestType wfmDoneUpdateStatusRequest = this.getRequestHolder().peekFromEndWithOffset(
				UpdateOrderStatusRequestType.class, 1);
		com.swisscom.esw.cols.ws.tibco.notify.AttributeType wfmDoneUpdateStatusRequestAttributes = wfmDoneUpdateStatusRequest.getOrderItem()
				.get(0).getAttributes();
		Assert.assertEquals(5, wfmDoneUpdateStatusRequestAttributes.getAttribute().size());
		Assert.assertEquals("WfmDoneDate", wfmDoneUpdateStatusRequestAttributes.getAttribute().get(1).getName());
		Assert.assertEquals("2000-01-13 01:02", wfmDoneUpdateStatusRequestAttributes.getAttribute().get(1).getValue());

		// Activate Alarm
		ServiceLocatorCdi.cdi(ColsOrderAlarmActivationService.class).activateAlarm(n20ConfirmOrder.getOrderId());

		Assert.assertEquals("checking n20ConfirmOrder", OrderState.FINISHED, n20ConfirmOrder.getState());
		Assert.assertEquals("confirm order should be finished", OrderState.FINISHED, s33PreOrderTibcoOrder.getState());
		Assert.assertEquals("main order should be ", OrderState.FINISHED, s33PreOrderTibcoOrder.getParentX().getState());
		Assert.assertEquals("main order should be ", OrderState.FINISHED, preOrder.getState());

		// There should be one access created due to the confirmation
		assertEquals(1, colsAccessRepository.count());

		// Check notification XML
		this.assertLastNotificationXml("ColsServiceIT_n20_notification.xml");

		ColsAccessService colsAccessService = ServiceLocatorCdi.cdi(ColsAccessService.class);
		List<ColsAccess> accesses = colsAccessService.searchAll();
		assertEquals(1, accesses.size());

		// Check values
		ColsAccess colsAccess = accesses.get(0);
		assertEquals("unrelated", colsAccess.getAccessType());
		assertEquals("123", colsAccess.getCustomerId());
		assertEquals("HighEndFiber", colsAccess.getServiceRealizationType());
		assertEquals("HighEnd", colsAccess.getCpeOption());
		assertEquals("0", colsAccess.getCustomerWindow());
		assertEquals("ServiceIdLogicalPhysicalCpe", colsAccess.getLogicalPhysicalCpe().getCfsIdLogicalPhysicalCpe());
		assertEquals("AC0000001", colsAccess.getAccessId());
		assertEquals("COL:ACC:1", colsAccess.getColsAccessId());
		assertEquals("ALL:SUB:1", colsAccess.getMscLeg().getMscLegId());
		assertEquals("ALL:SUB:2", colsAccess.getLogicalPhysicalCpe().getLogicalPhysicalCpeId());

		// Check Customer values
		assertEquals("UBS AG", colsAccess.getColsSite().getCompanyName());
		assertEquals("Bahnhofstrasse", colsAccess.getColsSite().getStreet());
		assertEquals("100", colsAccess.getColsSite().getHouseNumber());
		assertEquals("Bern", colsAccess.getColsSite().getCity());
		assertEquals("300800", colsAccess.getColsSite().getPostalCode());
		assertEquals("Peter Schuhmacher", colsAccess.getColsSite().getContactPerson());
		assertEquals("0792315847", colsAccess.getColsSite().getContactTelephone());
		assertEquals("Peter.Schuhmacher@bluewin.ch", colsAccess.getColsSite().getEmail());

		// Check Dealer values
		assertEquals("Martin Lala", colsAccess.getDealer().getName());
		assertEquals("033352352", colsAccess.getDealer().getPhone());
		assertEquals("033352353", colsAccess.getDealer().getFax());
		assertEquals("Martin.Lala@bluewin.ch", colsAccess.getDealer().getEmail());

		// Check Cpe Values
		assertEquals("CpeModel-1", colsAccess.getLogicalPhysicalCpe().getCpe().getCpeModel());
		assertEquals("CpeName-1", colsAccess.getLogicalPhysicalCpe().getCpe().getCpeName());
		assertEquals("CpeWanPortName-1", colsAccess.getLogicalPhysicalCpe().getCpe().getCpeWanPortName());
		assertEquals("CpeWanPortPluggable-1", colsAccess.getLogicalPhysicalCpe().getCpe().getCpeWanPortPluggable());
		assertEquals("cpeLanPortName1", colsAccess.getLogicalPhysicalCpe().getCpe().getCpeLanPortName());
		assertEquals("cpeLanPortPluggable1", colsAccess.getLogicalPhysicalCpe().getCpe().getCpeLanPortPluggable());

		// Check CableBoxData
		final MscHeLeg mscHeLeg = (MscHeLeg) colsAccess.getMscLeg();
		assertNotNull(mscHeLeg.getCableBoxData());
		assertEquals("CableBox-1", mscHeLeg.getCableBoxData().getCableBox());
		assertEquals("ContactPoint1-1", mscHeLeg.getCableBoxData().getContactPoint1());
		assertEquals("ContactPoint2-1", mscHeLeg.getCableBoxData().getContactPoint2());

		// Check ServiceEnabling
		assertEquals("FirstNet", colsAccess.getServiceEnablingType());
		assertEquals("2", colsAccess.getServiceEnablingClass());

		// CpeFinalizedStatus
		assertTrue(colsAccess.getLogicalPhysicalCpe().getCpeFinalizedStatus());
		
		//Check orderCreator the same as in create cols access order
		String createOrderCreatorUser = processCreate.getRequest().getOrderHeaderData().getOrderCreatorUser();
		String confirmOrderCreatorUser = processConfirm.getRequest().getOrderHeaderData().getOrderCreatorUser();
		assertEquals(createOrderCreatorUser, confirmOrderCreatorUser);

		checkCallTimeOutManageChangeRequest("EOE-0000002.010");
	}

	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml", inserts = { "testInputData/cpeData.xml", "testInputData/testUser.xml" })
	public void canCreate_n20_acceptCpe() throws ObjectNotFoundException {
		Order newColsOrder = OrderCreation.createOrder("ColsService_createColsAccess_assignCpe.xml");

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(newColsOrder);
	}

	// Loop through SystemInteractionEntry entries found by orderId (alaram deactivation: EOE-XXXX)
	// and check if it has a mangeChange itsm change with callTimeout = 90
	private void checkCallTimeOutManageChangeRequest(String subOrderId) {
		SystemInteractionEntryService systemInteractionEntryService = ServiceLocatorCdi.cdi(SystemInteractionEntryService.class);
		List<ISystemInteractionEntry> iSystemInteractionEntryList = systemInteractionEntryService.findByOrderId(subOrderId);

		boolean checkCallTimeOutInChangeManageRequest = false;
		for (ISystemInteractionEntry iSystemInteractionEntry : iSystemInteractionEntryList) {
			if ("OUTPUT".equals(iSystemInteractionEntry.getType()) && iSystemInteractionEntry.getXml().contains("manageChangeRequest>")) {
				ManageChangeRequest manageChangeRequest = XmlUtils.marshaller().unmarshal(iSystemInteractionEntry.getXml(),
						ManageChangeRequest.class);
				checkCallTimeOutInChangeManageRequest = manageChangeRequest.getTrackingInformation().getCallTimeout() != null;
			}
		}
		Assert.assertTrue(" The TrackingInformation of the request does not contain callTimeout parameter",
				checkCallTimeOutInChangeManageRequest);
	}

	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/CpeLanPortName_ForTranslationCheck.xml", "testInputData/testUser.xml" })
	public void canCreate_UpdateColsAccessCpe() throws ObjectNotFoundException, BusinessException {
		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel("cpeLanPortName1", "CpeModel-1", "HighEnd", true, false, null, null);

		new OoeOrderColsFacadeIT().mockNonExistingVLanServiceId();

		// Create a ColsAccess order
		canCreate_n20();

		// Create a ColsNet order
		String newColsOrderXml = new IOUtil().loadTextFromUrl(JuUrl.resourceRelativeTo("ColsService_newColsNetHybridTrue.xml",
				this.getClass()));
		Order newColsOrder = XmlUtils.marshaller().unmarshal(newColsOrderXml, Order.class);

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(newColsOrder);

		// Get the order that was implicitly created by the ColsService
		OrderX colsNetOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003", this.getModifiedSetter()));

		OrderDetails od = colsNetOrder.process();
		Assert.assertFalse(od.isHasErrors());

		// Make sure order is not finished yet
		Assert.assertEquals(OrderState.IN_PROGRESS, colsNetOrder.getState());

		// Simulate notification
		OrderType submittedOrder = this.pollLastRequest(OrderType.class);
		new LcsNotificationHandler().simulateNotification(submittedOrder);

		// Make sure the order is finished now
		Assert.assertEquals(OrderState.FINISHED, colsNetOrder.getState());

		// Create an Extend order
		String extendNetOrderXml = new IOUtil().loadTextFromUrl(JuUrl.resourceRelativeTo(
				"ColsService_extendColsNet_updateColsAccessCpe.xml",
				this.getClass()));
		Order extendNetOrder = XmlUtils.marshaller().unmarshal(extendNetOrderXml, Order.class);

		ColsAccess colsAccess = ServiceLocatorCdi.cdi(ColsAccessService.class).searchByColsAccessId("COL:ACC:1");
		SortedSet<CpeLan> colsAccessCpePortsList = colsAccess.getLogicalPhysicalCpe().getCpe().getCpeLanList();
		Assert.assertEquals(1, colsAccessCpePortsList.size());
		Assert.assertEquals("cpeLanPortPluggable1", colsAccessCpePortsList.first().getCpeLanPortPluggable());
		Assert.assertEquals("cpeLanPortName1", colsAccessCpePortsList.first().getCpeLanPortName());

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(extendNetOrder);

		// Get the order that was implicitly created by the ColsService
		OrderX order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000004", this.getModifiedSetter()));
		OrderX changeCpeOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000004.001", this.getModifiedSetter()));
		OrderX isHybridOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000004.003", this.getModifiedSetter()));
		OrderX s20Order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000004.004", this.getModifiedSetter()));
		OrderX n10Order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000004.005", this.getModifiedSetter()));
		OrderX s30ServiceOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000004.006", this.getModifiedSetter()));
		OrderX s31Order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000004.007", this.getModifiedSetter()));
		OrderX s36Order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000004.008", this.getModifiedSetter()));
		OrderX s32Order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000004.009", this.getModifiedSetter()));

		// Extend orders
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, changeCpeOrder.getState());
		Assert.assertEquals(OrderState.WAITING, isHybridOrder.getState());
		Assert.assertEquals(OrderState.WAITING, s20Order.getState());
		Assert.assertEquals(OrderState.WAITING, n10Order.getState());
		Assert.assertEquals(OrderState.WAITING, s30ServiceOrder.getState());
		Assert.assertEquals(OrderState.WAITING, s31Order.getState());
		Assert.assertEquals(OrderState.WAITING, s36Order.getState());
		Assert.assertEquals(OrderState.WAITING, s32Order.getState());

		// **** Update CPE ***///
		ColsAccessService colsAccessService = ServiceLocatorCdi.cdi(ColsAccessService.class);
		ColsAccess aColsAccess = colsAccessService.searchByColsAccessId("COL:ACC:1");
		SendOeColsOrder sendOeColsOrder = ServiceLocatorCdi.cdi(SendOeColsOrder.class);
		sendOeColsOrder.updateCpe(aColsAccess, "cpeLanPortName1", "cpeLanPortPluggable1", new Date(), null, getMetaData());

		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.WAITING, isHybridOrder.getState());
		Assert.assertEquals(OrderState.WAITING, s20Order.getState());
		Assert.assertEquals(OrderState.WAITING, n10Order.getState());
		Assert.assertEquals(OrderState.WAITING, s30ServiceOrder.getState());
		Assert.assertEquals(OrderState.WAITING, s31Order.getState());
		Assert.assertEquals(OrderState.WAITING, s36Order.getState());
		Assert.assertEquals(OrderState.WAITING, s32Order.getState());

		// Get the order Update Cols Access
		OrderX orderUpdate = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005", this.getModifiedSetter()));
		Assert.assertNotNull("order not found", order);

		OrderX manuallyAssignCpeOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.001", getModifiedSetter()));
		OrderX assignCpeMaterialsOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.002", getModifiedSetter()));
		OrderX assignInstallRfsDateOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.003", getModifiedSetter()));
		OrderX cpeInstallationOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.004", getModifiedSetter()));
		OrderX colAccessCopeOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.005", getModifiedSetter()));
		OrderX logicalPhysicalCpeCopeOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.006", getModifiedSetter()));
		OrderX cpeInstallationDoneOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.007", getModifiedSetter()));
		OrderX retrivalNetCrakerOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.008", getModifiedSetter()));

		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, manuallyAssignCpeOrder.getState());
		Assert.assertEquals(OrderState.WAITING, assignCpeMaterialsOrder.getState());
		Assert.assertEquals(OrderState.WAITING, assignInstallRfsDateOrder.getState());
		Assert.assertEquals(OrderState.WAITING, cpeInstallationOrder.getState());
		Assert.assertEquals(OrderState.WAITING, cpeInstallationDoneOrder.getState());
		Assert.assertEquals(OrderState.WAITING, colAccessCopeOrder.getState());
		Assert.assertEquals(OrderState.WAITING, logicalPhysicalCpeCopeOrder.getState());
		Assert.assertEquals(OrderState.WAITING, retrivalNetCrakerOrder.getState());

		od = orderUpdate.process();
		Assert.assertFalse(od.isHasErrors());

		// Simulate that the cpe has been manually assigned.

		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrder = nbOrderService.findByOrderId("EOE-0000005");
		Assert.assertNotNull(nbOrder);
		/********************** ASSIGN CPE ****************************/
		ColsResponseAccessUpdate aResponse = ((ColsResponseAccessUpdate) nbOrder.getColsProcess().getResponse());
		ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).mockSetCpeData("EOE-0000005.001", "CpeModel-2", "cpeWanPortName",
				"cpeWanPortNumber", "cpeLanPortPluggable1", "cpeLanPortName1", "comment", getMetaData());

		// Check Response
		SortedSet<CpeLan> cpePortsList = aResponse.getCpe().getCpeLanList();
		Assert.assertEquals(1, cpePortsList.size());
		Assert.assertEquals("cpeLanPortPluggable1", cpePortsList.first().getCpeLanPortPluggable());
		Assert.assertEquals("cpeLanPortName1", cpePortsList.first().getCpeLanPortName());

		// Set multiple plugs
		Cpe cpeResponse = aResponse.getCpe();
		addMultipleCpePorts(cpeResponse);

		ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).setCpeDataDone("EOE-0000005.001", getMetaData());

		Assert.assertEquals(OrderState.FINISHED, manuallyAssignCpeOrder.getState());

		// Check response cpe data
		cpePortsList = cpeResponse.getCpeLanList();
		Assert.assertEquals(5, cpePortsList.size());
		Assert.assertEquals("CpeModel-2", cpeResponse.getCpeModel());
		Assert.assertEquals("cpeLanPortName1", cpeResponse.getCpeLanPortName());
		Assert.assertEquals("cpeLanPortPluggable1", cpeResponse.getCpeLanPortPluggable());

		cpePortsList = aResponse.getCpe().getCpeLanList();
		Assert.assertEquals(5, cpePortsList.size());
		Assert.assertEquals("cpeLanPortPluggable1", cpePortsList.first().getCpeLanPortPluggable());
		Assert.assertEquals("cpeLanPortName1", cpePortsList.first().getCpeLanPortName());
		Assert.assertEquals("cpeLanPortPluggable5", cpePortsList.last().getCpeLanPortPluggable());
		Assert.assertEquals("cpeLanPortName5", cpePortsList.last().getCpeLanPortName());

		Assert.assertEquals(OrderState.IN_PROGRESS, assignCpeMaterialsOrder.getState());
		Assert.assertEquals(OrderState.WAITING, cpeInstallationOrder.getState());
		Assert.assertEquals(OrderState.WAITING, cpeInstallationDoneOrder.getState());
		Assert.assertEquals(OrderState.WAITING, colAccessCopeOrder.getState());
		Assert.assertEquals(OrderState.WAITING, logicalPhysicalCpeCopeOrder.getState());
		Assert.assertEquals(OrderState.WAITING, retrivalNetCrakerOrder.getState());

		ColsRequestAccessUpdate aRequest = ((ColsRequestAccessUpdate) nbOrder.getColsProcess().getRequest());
		setInstallRfsDate(orderUpdate, aRequest.getRfsDate(), "");

		// Simulate that the cpe materials has been assigned.
		// First we set the CPE in the response so as to make the cpe installation work
		nbOrder = nbOrderService.findByOrderId("EOE-0000005");
		Assert.assertNotNull(nbOrder);
		aResponse = ((ColsResponseAccessUpdate) nbOrder.getColsProcess().getResponse());
		aResponse.getTaskStatus().setCpeMaterialDone(true);
		aResponse.getTaskStatus().setRfsDateDone(true);
		ServiceLocatorCdi.cdi(UpdateColsAccessProcessService.class).update((ColsProcessAccessUpdate) nbOrder.getColsProcess());

		// Simulate WFM Response
		int wfmId = Integer.valueOf(aResponse.getWfmId());
		ServiceLocatorCdi.cdi(ColsWfmSimulator.class).simulateResponse(wfmId, ColsWfmSimulator.CLOSED);

		// The cpe installation finishes immediatelly because the simulation of wfm answers automatically.
		Assert.assertEquals(OrderState.FINISHED, manuallyAssignCpeOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, assignCpeMaterialsOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, cpeInstallationOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, colAccessCopeOrder.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, logicalPhysicalCpeCopeOrder.getState());
		Assert.assertEquals(OrderState.WAITING, cpeInstallationDoneOrder.getState());
		Assert.assertEquals(OrderState.WAITING, retrivalNetCrakerOrder.getState());

		// Simulate CoPE Notification
		new LcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		// Update ColsAccess is finished
		Assert.assertEquals(OrderState.FINISHED, manuallyAssignCpeOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, assignCpeMaterialsOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, cpeInstallationOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, cpeInstallationDoneOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, colAccessCopeOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, logicalPhysicalCpeCopeOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, retrivalNetCrakerOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, orderUpdate.getState());

		// set tha CpeLanPortName is done
		mockCpeLanPortNameDone("EOE-0000004");

		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, changeCpeOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, isHybridOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, s20Order.getState());
		Assert.assertEquals(OrderState.FINISHED, n10Order.getState());
		Assert.assertEquals(OrderState.FINISHED, s30ServiceOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, s31Order.getState());

		// Cope orders in progress
		Assert.assertEquals(OrderState.IN_PROGRESS, s36Order.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, s32Order.getState());

		// Check sent order for createN50
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_n50_changeCpeType_copeOrder.xml");

		// Make sure order is not finished yet
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());

		// Simulate notification
		submittedOrder = this.pollLastRequest(OrderType.class);
		new LcsNotificationHandler().simulateNotification(submittedOrder);

		// Check notification XML
		this.assertLastNotificationXml("ColsServiceIT_n50_changeCpeType_notification.xml");

		L2ServiceConnectivityService aL2ServiceConnectivityService = ServiceLocatorCdi.cdi(L2ServiceConnectivityService.class);
		List<L2ServiceConnectivity> allConnectivities = aL2ServiceConnectivityService.searchAll();
		assertEquals(1, allConnectivities.size());
		// this attribute needs no testing : not used any more
		// assertEquals(true, allConnectivities.get(0).getColsAccessChanged());

		// Check create ColsNorthboundOrder
		nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		nbOrder = null;
		nbOrder = nbOrderService.findByOrderId("EOE-0000004");
		Assert.assertNotNull(nbOrder);
		Assert.assertEquals(9, nbOrder.getSuborders().size());

		// Make sure the Extend order is finished now
		Assert.assertEquals(OrderState.FINISHED, order.getState());
		Assert.assertEquals(OrderState.FINISHED, changeCpeOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, isHybridOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, s20Order.getState());
		Assert.assertEquals(OrderState.FINISHED, n10Order.getState());
		Assert.assertEquals(OrderState.FINISHED, s30ServiceOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, s31Order.getState());
		Assert.assertEquals(OrderState.FINISHED, s36Order.getState());
		Assert.assertEquals(OrderState.FINISHED, s32Order.getState());

		Cpe cpeColsAccess = colsAccess.getLogicalPhysicalCpe().getCpe();
		colsAccessCpePortsList = cpeColsAccess.getCpeLanList();
		Assert.assertEquals(5, colsAccessCpePortsList.size());
		Assert.assertEquals("CpeModel-2", cpeColsAccess.getCpeModel());
		Assert.assertEquals("cpeLanPortName1", cpeColsAccess.getCpeLanPortName());
		Assert.assertEquals("cpeLanPortPluggable1", cpeColsAccess.getCpeLanPortPluggable());
		Assert.assertEquals("cpeLanPortPluggable5", cpePortsList.last().getCpeLanPortPluggable());
		Assert.assertEquals("cpeLanPortName5", cpePortsList.last().getCpeLanPortName());

		// Check ColsUpdateHistory attributes
		ColsUpdateHistoryRepository updateHistoryRepo = ServiceLocatorCdi.cdi(ColsUpdateHistoryRepository.class, QualifierUtils.COLS_ANNO);
		List<ColsUpdateHistory> updates = updateHistoryRepo.findByColsEntityId(aColsAccess.getColsAccessId());
		assertEquals(1, updates.size());
		SortedSet<AttributeUpdate> attributes = updates.get(0).getAttributes();
		assertEquals(9, attributes.size());
		assertEquals("COL:ACC:1", updates.get(0).getColsEntityId());
		assertEquals("UnitTest", updates.get(0).getChangeUser());
		assertEquals("CpeLanPortName2", attributes.first().getAttributeName());
		assertEquals("", attributes.first().getOldValue());
		assertEquals("cpeLanPortName2", attributes.first().getNewValue());
		assertEquals("CpeModel", attributes.last().getAttributeName());
		assertEquals("CpeModel-1", attributes.last().getOldValue());
		assertEquals("CpeModel-2", attributes.last().getNewValue());
	}

	@Test
	@TodoFixTest(value = "Remove commit as soon as migration to JPA is through", developer = Developer.Martin)
	// Disable export as XML in log is unpredictable
	public void canCreate_UpdateColsPortMoveEcEcp() throws Exception {
		testContext().eoeDefault();

		// first create an ColsAccess HighEnd
		canCreate_n20();
		ColsAccessRepository colsAccessRepo = ServiceLocatorCdi.cdi(ColsAccessRepository.class, QualifierUtils.COLS_ANNO);
		List<ColsAccess> colsAccessList = colsAccessRepo.findAll();

		Assert.assertEquals(1, colsAccessList.size());
		MscHeLeg mscHeLeg = (MscHeLeg) colsAccessList.get(0).getMscLeg();

		CrudService crudService = ServiceLocatorCdi.cdi(CrudService.class);

		// set up simulate startDate / endDate
		Date startDate = getModifiedSetter().getDate();
		Date endDate = getSimulateEndDate();

		// create EcPort and EcPort 2 and persist
		NePort anEcPort = getFirstEcPort(startDate, endDate);
		anEcPort = (NePort) crudService.create(anEcPort);
		mscHeLeg.setNePort(anEcPort);
		String mscLegCfsId = mscHeLeg.getCfsId();

		Order order = createPortMoveEcEcpOrder();

		Assert.assertTrue(ColsServiceOrderMatcher.isPortMoveEcEcp(order));

		// submit the order
		new ColsService().submitOrder(order);

		debug().commitToDb();

		// Get the order that was implicitly created by the ColsService
		OrderX orderX = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003", this.getModifiedSetter()));

		// reprocessOrder(orderX.getOrderId());

		// check if notification to the caller ?
		this.assertLastNotificationXml("ColsServiceIT_portMoveEcEcp_start_notification.xml");

		// check State
		Assert.assertEquals(OrderState.IN_PROGRESS, orderX.getState());

		// simulate response from NC
		ColsRequest portMoveUpdateRequest = new ColsRequest();
		portMoveUpdateRequest.setHeader(new ColsRequest.Header());
		portMoveUpdateRequest.setArguments(new ColsRequest.Arguments());

		portMoveUpdateRequest.getHeader().setServiceName(ColsConstants.COLS_SERVICE_NAME_PORTMOVE_REQUEST);
		portMoveUpdateRequest.getHeader().setServiceType(ColsConstants.COLS_SERVICE_TYPE_NOTIFY);
		portMoveUpdateRequest.getHeader().setCallerApplication("Netcracker");
		portMoveUpdateRequest.getHeader().setCallDateTime(XmlUtils.asXMLGregorianCalendar(EswDateUtils.getNow()));

		Argument argStatus = new Argument();
		argStatus.setName("Status");
		argStatus.setValue("succeeded");

		Argument argCorrelationId = new Argument();
		argCorrelationId.setName("correlationID");
		argCorrelationId.setValue("EOE-0000003");

		Argument argNetworkElement = new Argument();
		argNetworkElement.setName("TargetNetworkElement");
		argNetworkElement.setValue("ip-moet640-s-ca-01");

		Argument argInterface = new Argument();
		argInterface.setName("TargetInterface");
		argInterface.setValue("GigabitEthernet0/3/0/9");

		Argument argMscLegId = new Argument();
		argMscLegId.setName("MSCLegId");
		argMscLegId.setValue(mscLegCfsId);

		portMoveUpdateRequest.getArguments().getArgument().add(argStatus);
		portMoveUpdateRequest.getArguments().getArgument().add(argCorrelationId);
		portMoveUpdateRequest.getArguments().getArgument().add(argNetworkElement);
		portMoveUpdateRequest.getArguments().getArgument().add(argInterface);
		portMoveUpdateRequest.getArguments().getArgument().add(argMscLegId);

		// simulating NC sends notification with status=inProcess
		for (Argument arg : portMoveUpdateRequest.getArguments().getArgument()) {
			if ("Status".equals(arg.getName())) {
				arg.setValue("bla bla bla");
			}
		}
		new ColsService().callService(portMoveUpdateRequest);

		// the order should still be in process
		// reprocessOrder(orderX.getOrderId());
		Assert.assertEquals(OrderState.IN_PROGRESS, orderX.getState());

		// simulating now NC sends *succeeded* notification
		for (Argument arg : portMoveUpdateRequest.getArguments().getArgument()) {
			if ("Status".equals(arg.getName())) {
				arg.setValue("succeeded");
			}
		}
		new ColsService().callService(portMoveUpdateRequest);

		// now the order should finish:
		Assert.assertEquals(OrderState.FINISHED, orderX.getState());

		// check if notification to the caller ?
		this.assertLastNotificationXml("ColsServiceIT_portMoveEcEcp_finish_notification.xml");

		colsAccessList = colsAccessRepo.findAll();
		Assert.assertEquals(1, colsAccessList.size());
		ColsAccess aColsAccess = colsAccessList.get(0);

		ColsUpdateHistoryRepository updateHistoryRepo = ServiceLocatorCdi.cdi(ColsUpdateHistoryRepository.class, QualifierUtils.COLS_ANNO);
		List<ColsUpdateHistory> updates = updateHistoryRepo.findByColsEntityId(aColsAccess.getColsAccessId());
		assertEquals(1, updates.size());
		SortedSet<AttributeUpdate> attributes = updates.get(0).getAttributes();
		assertEquals(2, attributes.size());
		assertEquals("COL:ACC:1", updates.get(0).getColsEntityId());
		assertEquals("ITSM", updates.get(0).getChangeUser());
		assertEquals("NodeName", attributes.first().getAttributeName());
		assertEquals("ecName", attributes.first().getOldValue());
		assertEquals("ip-moet640-s-ca-01", attributes.first().getNewValue());
		assertEquals("NodePort", attributes.last().getAttributeName());
		assertEquals("ecPort", attributes.last().getOldValue());
		assertEquals("GigabitEthernet0/3/0/9", attributes.last().getNewValue());
	}

	/**
	 * Test case for a new N20 with 3 pluggables from North Bound - GUI simulation deletes them all.
	 */
	@Test
	public void canCreate_n20_with_3Pluggables_GUI_DELETES_ALL_CPELANS() {
		OrderX preOrder = this.submitN20(false);

		// Get SubOrders we will make checks on
		OrderX s33PreOrderTibcoOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_S33_PRE_ORDER.getType());

		// Process the order to make sure it's processed without errors
		OrderDetails od = preOrder.process();
		Assert.assertFalse(od.isHasErrors());

		// Make sure the S33 suborder is waiting (for the CPE Model assignment)
		Assert.assertEquals(OrderState.WAITING, s33PreOrderTibcoOrder.getState());

		// The SubOrder COLS_ORDER_ASSIGN_CPE should be in state READY
		OrderX assignCpeOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE.getType());
		Assert.assertEquals(OrderState.IN_PROGRESS, assignCpeOrder.getState());

		// Simulate the GUI Assign of CPE Data. GUI calls same service as below:
		setCpeDataWithNoPlugs(preOrder, false);

		Assert.assertEquals(OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getState());

		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_n20_copeOrder_no_plugs.xml");

		// Check Main Order ready notification XML - the first response should still show the initial 3 plugs
		this.assertLastNotificationXml("ColsServiceIT_n20_main_ready_notification.xml");

		// The main order should be in progress
		Assert.assertEquals(OrderState.IN_PROGRESS, preOrder.getState());

		// S33 Preorder should be in progress, waiting for manual update of the data
		Assert.assertEquals(OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getState());

		// find the ColAccessProcess we just created:
		ColsProcessAccessCreateHighEnd aProcess = preOrder.getDataObjectNotNull(ColsProcessAccessCreateHighEnd.class, true);
		CreateColsAccessProcessService aService = ServiceLocatorCdi.cdi(CreateColsAccessProcessService.class);
		try {
			// to ensure that the persisted process is retrieved, get the process once more, this time by Id
			aProcess = aService.searchById(aProcess.getId());
		} catch (Exception ex) {
			throw new ServiceDbRuntimeException("Could not locate CreateColsAccessProcess for process id " + aProcess.getId(), ex);
		}

		// Check CpeLanList on the Request ---------------------------------------------------

		// get the Cpe on the Request from the Process
		Cpe cpe = aProcess.getRequest().getCpe();
		SortedSet<CpeLan> cpeLanList = cpe.getCpeLanList();

		// there should be 3 items in the CpeLanList
		Assert.assertEquals(3, cpeLanList.size());

		for (CpeLan cpeLan : cpeLanList) {
			Assert.assertNotNull(cpeLan.getCpeLanPortName());
		}

		// Check CpeLanList on the Response ---------------------------------------------------

		// get the Cpe on the Response from the Process - it should have nothing in the CpeLanList
		cpe = aProcess.getResponse().getCpe();
		Assert.assertTrue(cpe.getCpeLanList().isEmpty());

		// assertEquals(1, ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).compareOrdersCount());
	}

	/**
	 * Test case for a new N20 with 3 pluggables from North Bound that get updated by GUI simulation.
	 */
	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/cpeData.xml" })
	public void canCreate_n20_with_3Pluggables_GUI_UPDATES_LANPORTNAMES() {
		OrderX preOrder = this.submitN20(false);

		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel("cpeLanPortName1", "CpeModel-1", "HighEnd", false, false, null, null);
		CpeModelTestdataCreator.createCpeMaterial("HighEnd", "CpeModel-1");
		CpeModelTestdataCreator.createCpeMaterial("HighEnd", "WS-C4900M");

		// Get SubOrders we will make checks on
		OrderX s33PreOrderTibcoOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_S33_PRE_ORDER.getType());

		// Process the order to make sure it's processed without errors
		OrderDetails od = preOrder.process();
		Assert.assertFalse(od.isHasErrors());

		// Make sure the S33 suborder is waiting (for the CPE Model assignment)
		Assert.assertEquals(OrderState.WAITING, s33PreOrderTibcoOrder.getState());

		// The SubOrder COLS_ORDER_ASSIGN_CPE should be in state READY
		OrderX assignCpeOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE.getType());
		Assert.assertEquals(OrderState.IN_PROGRESS, assignCpeOrder.getState());

		// find the ColAccessProcess we just created:
		ColsProcessAccessCreateHighEnd aProcess = preOrder.getDataObjectNotNull(ColsProcessAccessCreateHighEnd.class, true);
		CreateColsAccessProcessService aService = ServiceLocatorCdi.cdi(CreateColsAccessProcessService.class);
		try {
			// to ensure that the persisted process is retrieved, get the process once more, this time by Id
			aProcess = aService.searchById(aProcess.getId());
		} catch (Exception ex) {
			throw new ServiceDbRuntimeException("Could not locate CreateColsAccessProcess for process id " + aProcess.getId(), ex);
		}

		// Check CpeLanList on the Request ---------------------------------------------------

		// get the Cpe on the Request from the Process

		Cpe cpe = aProcess.getRequest().getCpe();
		SortedSet<CpeLan> reqCpeLanList = cpe.getCpeLanList();

		// there should be 3 items in the CpeLanList
		Assert.assertEquals(3, reqCpeLanList.size());

		for (CpeLan cpeLan : reqCpeLanList) {
			Assert.assertNotNull(cpeLan.getCpeLanPortName());
		}

		// Check CpeLanList on the Response ---------------------------------------------------

		// get the Cpe on the Response from the Process
		cpe = aProcess.getResponse().getCpe();
		SortedSet<CpeLan> respCpeLanList = cpe.getCpeLanList();

		// there should be 3 items in the CpeLanList
		Assert.assertEquals(3, respCpeLanList.size());

		// -------------------------------------------------------------------------------------
		// Simulate the GUI Assign of CPE Data. GUI calls same service as below:
		setCpeDataWithMultiplePlug(preOrder, false);

		Assert.assertEquals(OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getState());

		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_n20_copeOrder_gui_updates.xml");

		// Check Main Order ready notification XML
		this.assertLastNotificationXml("ColsServiceIT_n20_main_ready_notification.xml");

		// The main order should be in progress
		Assert.assertEquals(OrderState.IN_PROGRESS, preOrder.getState());

		// S33 Preorder should be in progress, waiting for manual update of the data
		Assert.assertEquals(OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getState());

		// Check CpeLanList on the Response ---------------------------------------------------

		// get the Cpe on the Response from the Process
		cpe = aProcess.getResponse().getCpe();
		respCpeLanList = cpe.getCpeLanList();

		// there should be 5 items in the CpeLanList
		Assert.assertEquals(5, respCpeLanList.size());

		for (CpeLan cpeLan : respCpeLanList) {
			Assert.assertNotNull(cpeLan.getCpeLanPortName());
		}

		check5Pluggables(respCpeLanList);

		// Check CpeLanList on the Request ---------------------------------------------------

		// get the Cpe again from the Request to make sure that it did not change (that resp is not just a ref to req)

		cpe = aProcess.getRequest().getCpe();
		reqCpeLanList = cpe.getCpeLanList();

		// there should be 3 items in the CpeLanList
		Assert.assertEquals(3, reqCpeLanList.size());

		for (CpeLan cpeLan : reqCpeLanList) {
			Assert.assertNotNull(cpeLan.getCpeLanPortName());
		}

		// assertEquals(1, ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).compareOrdersCount());
	}

	/**
	 * Test case for a new N20 with 4 pluggables from North Bound - one gets deleted by GUI simulation.
	 */
	@Test
	public void canCreate_n20_with_4Pluggables_GUI_DELETES_1CPELAN() {
		OrderX preOrder = this.submitN20_4Plugs();

		// Get SubOrders we will make checks on
		OrderX s33PreOrderTibcoOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_S33_PRE_ORDER.getType());

		// Process the order to make sure it's processed without errors
		OrderDetails od = preOrder.process();
		Assert.assertFalse(od.isHasErrors());

		// Make sure the S33 suborder is waiting (for the CPE Model assignment)
		Assert.assertEquals(OrderState.WAITING, s33PreOrderTibcoOrder.getState());

		// The SubOrder COLS_ORDER_ASSIGN_CPE should be in state READY
		OrderX assignCpeOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE.getType());
		Assert.assertEquals(OrderState.IN_PROGRESS, assignCpeOrder.getState());

		// Simulate the GUI Assign of CPE Data. GUI calls same service as below:
		setCpeDataWithMultiplePlug(preOrder, false);

		Assert.assertEquals(OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getState());

		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_n20_copeOrder.xml");

		// Check Main Order ready notification XML - the first response should still show 4 plugs
		this.assertLastNotificationXml("ColsServiceIT_n20_main_ready_notification_4plug.xml");

		// The main order should be in progress
		Assert.assertEquals(OrderState.IN_PROGRESS, preOrder.getState());

		// S33 Preorder should be in progress, waiting for manual update of the data
		Assert.assertEquals(OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getState());

		// find the ColAccessProcess we just created:
		ColsProcessAccessCreateHighEnd aProcess = preOrder.getDataObjectNotNull(ColsProcessAccessCreateHighEnd.class, true);
		CreateColsAccessProcessService aService = ServiceLocatorCdi.cdi(CreateColsAccessProcessService.class);
		try {
			// to ensure that the persisted process is retrieved, get the process once more, this time by Id
			aProcess = aService.searchById(aProcess.getId());
		} catch (Exception ex) {
			throw new ServiceDbRuntimeException("Could not locate ColsProcessAccessCreateHighEnd for process id " + aProcess.getId(), ex);
		}

		// Check CpeLanList on the Request ---------------------------------------------------

		// get the Cpe on the Request from the Process
		Cpe cpe = aProcess.getRequest().getCpe();
		SortedSet<CpeLan> cpeLanList = cpe.getCpeLanList();

		// there should be 4 items in the CpeLanList
		Assert.assertEquals(4, cpeLanList.size());

		for (CpeLan cpeLan : cpeLanList) {
			Assert.assertNotNull(cpeLan.getCpeLanPortName());
		}

		// Check CpeLanList on the Response ---------------------------------------------------

		// get the Cpe on the Response from the Process
		cpe = aProcess.getResponse().getCpe();
		cpeLanList = cpe.getCpeLanList();

		// there should be 3 items in the CpeLanList
		Assert.assertEquals(5, cpeLanList.size());

		for (CpeLan cpeLan : cpeLanList) {
			Assert.assertNotNull(cpeLan.getCpeLanPortName());
		}

		check5Pluggables(cpeLanList);

		// assertEquals(1, ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).compareOrdersCount());
	}

	@Test
	public void canCreate_n20_with_NoCpe() {
		canCreate_n20_with_NoCpe(false);
	}

	/**
	 * Test case for a new N20 with No pluggables from North Bound and 3 pluggable added by GUI simulation.
	 */
	@Test
	public void canCreate_n20_with_NoPluggables_GUI_ADDS_3CPELANS() {

		OrderX preOrder = this.submitN20NoPlugs();

		// Get SubOrders we will make checks on
		OrderX s33PreOrderTibcoOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_S33_PRE_ORDER.getType());

		// Process the order to make sure it's processed without errors
		OrderDetails od = preOrder.process();
		Assert.assertFalse(od.isHasErrors());

		// Make sure the S33 suborder is waiting (for the CPE Model assignment)
		Assert.assertEquals(OrderState.WAITING, s33PreOrderTibcoOrder.getState());

		// The SubOrder COLS_ORDER_ASSIGN_CPE should be in state READY
		OrderX assignCpeOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE.getType());
		Assert.assertEquals(OrderState.IN_PROGRESS, assignCpeOrder.getState());

		// Simulate the GUI Assign of CPE Data. GUI calls same service as below:
		setCpeDataWithMultiplePlug(preOrder, false);

		Assert.assertEquals(OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getState());

		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_n20_copeOrder.xml");

		// below should change to this.assertLastNotificationXml("ColsServiceIT_n20_main_ready_notification.xml");
		// This is actually the first notification which it happens at the beginning of the order thus it does not have the cpe changes.
		// Check Main Order ready notification XML - there should be no plugs in the first response
		this.assertLastNotificationXml("ColsServiceIT_n20_main_ready_notification_no_plugs.xml");

		// The main order should be in progress
		Assert.assertEquals(OrderState.IN_PROGRESS, preOrder.getState());

		// S33 Preorder should be in progress, waiting for manual update of the data
		Assert.assertEquals(OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getState());
		OrderType submittedOrder = this.pollLastRequest(OrderType.class);
		new LcsNotificationHandler().simulateNotification(submittedOrder, NotificationType.SUBMIT_ORDER_ACK);

		new LcsNotificationHandler().simulateNotification(submittedOrder);

		// find the ColAccessProcess we just created:
		ColsProcessAccessCreateHighEnd aProcess = preOrder.getDataObjectNotNull(ColsProcessAccessCreateHighEnd.class, true);
		CreateColsAccessProcessService aService = ServiceLocatorCdi.cdi(CreateColsAccessProcessService.class);
		try {
			// to ensure that the persisted process is retrieved, get the process once more, this time by Id
			aProcess = aService.searchById(aProcess.getId());
		} catch (Exception ex) {
			throw new ServiceDbRuntimeException("Could not locate ColsProcessAccessCreateHighEnd for process id " + aProcess.getId(), ex);
		}

		// Check that Cpe Is Null on the Request ----------------------------------------------

		Assert.assertNotNull(aProcess.getRequest().getCpe());

		// Check CpeLanList on the Response ---------------------------------------------------

		// get the Cpe on the Response from the Process
		Cpe cpe = aProcess.getResponse().getCpe();
		SortedSet<CpeLan> cpeLanList = cpe.getCpeLanList();

		// there should be 5 items in the CpeLanList
		Assert.assertEquals(5, cpeLanList.size());

		for (CpeLan cpeLan : cpeLanList) {
			Assert.assertNotNull(cpeLan.getCpeLanPortName());
		}

		check5Pluggables(cpeLanList);

		// assertEquals(1, ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).compareOrdersCount());
	}

	@Test
	public void canSubmitRevised_ColsRequestAccessCreateHighEnd() throws BusinessException {

		// Load an order with a missing attribute (invalid) but is from a system that is not REVISABLE
		// and hence the order should not be set to TO_REVISE but to REJECTED
		Order preOrder = this.loadColsOrder("ColsService_newColsAccess_no_plugs_ToRejectOrRevise.xml");
		
		// Set the OrderCreatorApplication Dynamically based on the use case
		preOrder.getOrderHeader().setOrderCreatorApplication("CDT"); // Non Revisable Application Order Creator System
		
		Assert.assertEquals("CDT", preOrder.getOrderHeader().getOrderCreatorApplication());
		
		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(preOrder);
		
		OrderX submitedOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", this.getModifiedSetter()));
		
		Assert.assertEquals(OrderState.REJECTED, submitedOrder.getState());
	}

	@Test
	public void canCreate_n20Confirm() {
		canCreate_n20Confirm(false, false);
	}

	@Test
	public void canCreate_n20Confirm_noCpe() {
		canCreate_n20Confirm(true, false);
	}

	@Test
	public void canCreate_n20Confirm_rfsDateDefinitiv() throws ObjectNotFoundException {
		canCreate_n20Confirm_rfsDateDefinitiv(false, false);
	}

	/**
	 * Delete ColsAccess without Connectivities
	 */
	@Test
	public void canCreate_N20Delete() throws Exception {

		canCreate_n20Confirm();

		createColsLeadTimes();

		OrderX n20 = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", getModifiedSetter()));
		OrderX s33PreOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.001", getModifiedSetter()));
		Assert.assertEquals("pre order should be ", OrderState.FINISHED, s33PreOrder.getState());
		Assert.assertEquals("main order should be ", OrderState.FINISHED, n20.getState());

		// find the ColAccess we just created:
		ColsProcessAccessCreateHighEnd proc = n20.getObjectEvaluatorX().getSingleNotNull(ColsProcessAccessCreateHighEnd.class,
				OrderObjectType.DATA);

		// we need this one to delete
		String colsAccessId = proc.getResponse().getColsAccessId();
		Assert.assertNotNull("colsAccessId should be available", colsAccessId);

		// try to delete the ColsAccess

		// Create the Order to hit the NorthBound

		Order deleteOrder = createDeleteColsAccessOrder();

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(deleteOrder);

		// Get the order
		OrderX order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003", this.getModifiedSetter()));

		OrderX checkIfConnectivitiesOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003.001", getModifiedSetter()));
		OrderX sendDeinstallationOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003.002", getModifiedSetter()));
		OrderX deinstallationDoneOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003.003", getModifiedSetter()));

		OrderX s33DeleteOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003.004", getModifiedSetter()));
		OrderX s34DeleteOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003.005", getModifiedSetter()));
		OrderX s35DeleteOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003.006", getModifiedSetter()));

		Assert.assertNotNull("delete order not found", order);

		OrderDetails od = order.process();
		Assert.assertFalse(od.isHasErrors());

		// Make sure order is not finished yet
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());

		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrder = nbOrderService.findByOrderId("EOE-0000003");
		Assert.assertNotNull(nbOrder);
		Assert.assertEquals(6, nbOrder.getSuborders().size());

		ColsProcessAccessDelete process = (ColsProcessAccessDelete) nbOrder.getColsProcess();

		// Simulate WFM Response
		int wfmId = Integer.valueOf(process.getResponse().getWfmId());
		ServiceLocatorCdi.cdi(ColsWfmSimulator.class).simulateResponse(wfmId, ColsWfmSimulator.CLOSED);

		// Get the ColsResponse from the WFM notification
		ColsResponse wfmColsResponse = this.getRequestHolder().pollRequest(ColsResponse.class);
		Boolean wfmEnabled = AttributeManagerUtil.getSystemPropertyBoolean("cols.wfm.ennabled");
		if (wfmEnabled) {
			Assert.assertTrue(wfmColsResponse.isSuccessful());
		}

		Assert.assertEquals(OrderState.FINISHED, deinstallationDoneOrder.getState());

		// Check sent order
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_n20_delete_copeOrder.xml");

		// Make sure order is now not yet finished (waiting for CoPE)
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, checkIfConnectivitiesOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, sendDeinstallationOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, deinstallationDoneOrder.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, s33DeleteOrder.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, s34DeleteOrder.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, s35DeleteOrder.getState());

		// Simulate CoPE Notification
		new LcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		// Check notification XML
		this.assertLastNotificationXml("ColsServiceIT_n20Delete_notification.xml");

		// Make sure order is now finished
		Assert.assertEquals(OrderState.FINISHED, order.getState());
		Assert.assertEquals(OrderState.FINISHED, s33DeleteOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, s34DeleteOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, s35DeleteOrder.getState());

		// Check information in System Interaction
		OrderX mainOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003", getModifiedSetter()));
		Assert.assertNotNull(mainOrder);

		SystemInteractionEntryService anInteractionService = ServiceLocatorCdi.cdi(SystemInteractionEntryService.class);
		List<ISystemInteractionEntry> interactionList = anInteractionService.findByOrderId(mainOrder.getOrderId());
		assertEquals(1, interactionList.size());

		// Check deletion attributes
		ColsAccessRepository colsAccessRepository = ServiceLocatorCdi.cdi(ColsAccessRepository.class, QualifierUtils.COLS_ANNO);
		List<ColsAccess> colsAccessesList = colsAccessRepository.findAll();
		assertEquals(1, colsAccessesList.size());
		assertNotNull(colsAccessesList.get(0).getDeleteDate());
		assertEquals("OH-CIH", colsAccessesList.get(0).getOrderCreator());
	}

	/**
	 * Delete ColsAccess with CpeOption = "noCpe"
	 */
	@Test
	public void canCreate_N20Delete_with_noCpe() throws Exception {

		canCreate_n20Confirm_noCpe();

		OrderX n20 = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", getModifiedSetter()));
		OrderX s33PreOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.001", getModifiedSetter()));
		Assert.assertEquals(OrderState.FINISHED, s33PreOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, n20.getState());

		ColsProcessAccessCreateHighEnd proc = n20.getObjectEvaluatorX().getSingleNotNull(ColsProcessAccessCreateHighEnd.class,
				OrderObjectType.DATA);
		String colsAccessId = proc.getResponse().getColsAccessId();
		Assert.assertNotNull(colsAccessId);

		Order deleteOrder = createDeleteColsAccessOrder();

		new ColsService().submitOrder(deleteOrder);

		// Get the order
		OrderX order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003", this.getModifiedSetter()));

		OrderX checkIfConnectivitiesOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003.001", getModifiedSetter()));
		OrderX s33DeleteOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003.002", getModifiedSetter()));
		OrderX s34DeleteOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003.003", getModifiedSetter()));
		OrderX s35DeleteOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003.004", getModifiedSetter()));

		Assert.assertNotNull(order);

		OrderDetails od = order.process();
		Assert.assertFalse(od.isHasErrors());

		// Make sure order is not finished yet
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, checkIfConnectivitiesOrder.getState());

		// Check sent order
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_n20_delete_copeOrder_noCpe.xml");

		// Make sure order is now not yet finished (waiting for CoPE)
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, checkIfConnectivitiesOrder.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, s33DeleteOrder.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, s34DeleteOrder.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, s35DeleteOrder.getState());

		// Simulate CoPE Notification
		new LcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		// Check notification XML
		this.assertLastNotificationXml("ColsServiceIT_n20Delete_notification.xml");

		// Make sure order is now finished
		Assert.assertEquals(OrderState.FINISHED, order.getState());
		Assert.assertEquals(OrderState.FINISHED, s33DeleteOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, s34DeleteOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, s35DeleteOrder.getState());

		// Check create ColsNorthboundOrder
		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrder = nbOrderService.findByOrderId("EOE-0000003");
		Assert.assertNotNull(nbOrder);
		Assert.assertEquals(4, nbOrder.getSuborders().size());

		// Check information in System Interaction
		OrderX mainOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003", getModifiedSetter()));
		Assert.assertNotNull(mainOrder);

		SystemInteractionEntryService anInteractionService = ServiceLocatorCdi.cdi(SystemInteractionEntryService.class);
		List<ISystemInteractionEntry> interactionList = anInteractionService.findByOrderId(mainOrder.getOrderId());
		assertEquals(1, interactionList.size());

		// Check deletion attributes
		ColsAccessRepository colsAccessRepository = ServiceLocatorCdi.cdi(ColsAccessRepository.class, QualifierUtils.COLS_ANNO);
		List<ColsAccess> colsAccessesList = colsAccessRepository.findAll();
		assertEquals(1, colsAccessesList.size());
		assertNotNull(colsAccessesList.get(0).getDeleteDate());
		assertEquals("OH-CIH", colsAccessesList.get(0).getOrderCreator());
	}

	/**
	 * Delete ColsAccess with Connectivities
	 */
	@Test
	public void canCreate_N20DeleteWithConnectivities() throws Exception {

		// create a connectivity -> which would then obviously need an L2Net and a ColsAccess
		ColsAccess colsAccess = this.testData().cols().newColsAccess("1", ColsAccessLegType.HIGH_END);
		colsAccess.setColsAccessId("ALL:SUB:1");
		ServiceLocatorCdi.cdi(ColsAccessService.class).update(colsAccess);

		L2Net l2Net = this.testData().cols().newL2Net("1", "CES");
		L2ServiceConnectivity connectivity = this.testData().cols().newL2ServiceConnectivity("1", colsAccess, l2Net);

		Assert.assertNotNull(colsAccess);
		Assert.assertNotNull(l2Net);
		Assert.assertNotNull(connectivity);

		// try to delete the ColsAccess

		// Create the Order to hit the NorthBound

		Order deleteOrder = createDeleteColsAccessOrder();

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(deleteOrder);
	}

	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml")
	public void canCreate_N20UpdateCpe() throws Exception {
		canCreate_N20UpdateCpe(false, false);
		ColsAccessService colsAccessService = ServiceLocatorCdi.cdi(ColsAccessService.class);
		List<ColsAccess> accesses = colsAccessService.searchAll();
		ColsAccess colsAccess = accesses.get(0);
		Assert.assertEquals(colsAccess.getColsAccessId(), "COL:ACC:1");
		Assert.assertTrue(colsAccess.getLogicalPhysicalCpe().getCpe().getCpeLanList().size() == 1);

		// Check ColsUpdateHistory attributes
		ColsUpdateHistoryRepository updateHistoryRepo = ServiceLocatorCdi.cdi(ColsUpdateHistoryRepository.class, QualifierUtils.COLS_ANNO);
		List<ColsUpdateHistory> updates = updateHistoryRepo.findByColsEntityId(colsAccess.getColsAccessId());
		assertEquals(1, updates.size());
		SortedSet<AttributeUpdate> attributes = updates.get(0).getAttributes();
		assertEquals(3, attributes.size());
		assertEquals("COL:ACC:1", updates.get(0).getColsEntityId());
		assertEquals("UnitTest", updates.get(0).getChangeUser());
		assertEquals("CpeLanPortName1", attributes.first().getAttributeName());
		assertEquals("cpeLanPortName1", attributes.first().getOldValue());
		assertEquals("cpeLanPortName", attributes.first().getNewValue());
		assertEquals("CpeModel", attributes.last().getAttributeName());
		assertEquals("CpeModel-1", attributes.last().getOldValue());
		assertEquals("CpeModel-2", attributes.last().getNewValue());
	}

	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml")
	public void canCreate_N20UpdateCpe_skipCope() throws Exception {
		canCreate_n20Confirm(false, false);

		OrderX n20PreOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", getModifiedSetter()));

		// find the ColAccess we just created:
		ColsProcessAccessCreateHighEnd proc = n20PreOrder.getObjectEvaluatorX().getSingleNotNull(ColsProcessAccessCreateHighEnd.class,
				OrderObjectType.DATA);

		// we need this one to update
		String colsAccessId = proc.getResponse().getColsAccessId();
		Assert.assertNotNull("colsAccessId should be available", colsAccessId);

		// Create the Order to hit the NorthBound
		Order updateOrder = createUpdateCpeColsAccessOrder(false);

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(updateOrder);

		OrderX order = this.getOrder("EOE-0000003");

		ServiceLocatorCdi.cdi(OeOrderRepository.class).findByOrderId("EOE-0000003.005").get(0)
				.setStatusId(Constants.OE_ORDER_STATE_SKIPPED);
		ServiceLocatorCdi.cdi(OeOrderRepository.class).findByOrderId("EOE-0000003.006").get(0)
				.setStatusId(Constants.OE_ORDER_STATE_SKIPPED);

		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrder = nbOrderService.findByOrderId("EOE-0000003");
		Assert.assertNotNull(nbOrder);

		ColsResponseAccessUpdate aResponse = ((ColsResponseAccessUpdate) nbOrder.getColsProcess().getResponse());

		MediantModel mediantModel = createMediantModel("MediantModel-1", 10);
		MediantSlot mediantSlot = createMediantSlot("MediantSlot-1");

		ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).setCpeData("EOE-0000003.001", "CpeModel-2", "cpeWanPortName", "cpeWanPortName",
				null, mediantModel, mediantSlot, null, null, "comment", getMetaData());
		/********************** ASSIGN CPE ****************************/
		ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).setCpeDataDone("EOE-0000003.001", getMetaData());
		/*************************************************************/

		aResponse.getTaskStatus().setCpeMaterialDone(true);

		setUpdateColsAccessOrderTasks(order, getMetaData());

		// Simulate WFM Response
		int wfmId = Integer.valueOf(aResponse.getWfmId());
		ServiceLocatorCdi.cdi(ColsWfmSimulator.class).simulateResponse(wfmId, ColsWfmSimulator.CLOSED);

		// Check ColsUpdateHistory attributes
		ColsUpdateHistoryRepository updateHistoryRepo = ServiceLocatorCdi.cdi(ColsUpdateHistoryRepository.class, QualifierUtils.COLS_ANNO);
		List<ColsUpdateHistory> updates = updateHistoryRepo.findByColsEntityId("COL:ACC:1");
		assertEquals(1, updates.size());
		SortedSet<AttributeUpdate> attributes = updates.get(0).getAttributes();
		assertEquals(2, attributes.size());
		assertEquals("COL:ACC:1", updates.get(0).getColsEntityId());
		assertEquals("UnitTest", updates.get(0).getChangeUser());
		assertEquals("CpeModel", attributes.first().getAttributeName());
		assertEquals("CpeModel-1", attributes.first().getOldValue());
		assertEquals("CpeModel-2", attributes.first().getNewValue());
		assertEquals("CpeWanPortName", attributes.last().getAttributeName());
		assertEquals("CpeWanPortName-1", attributes.last().getOldValue());
		assertEquals("cpeWanPortName", attributes.last().getNewValue());

	}

	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml")
	public void canCreate_N20UpdateCpe_noCpe() throws Exception {
		canCreate_N20UpdateCpe(true, false);

		// Check ColsUpdateHistory attributes
		ColsUpdateHistoryRepository updateHistoryRepo = ServiceLocatorCdi.cdi(ColsUpdateHistoryRepository.class, QualifierUtils.COLS_ANNO);
		List<ColsUpdateHistory> updates = updateHistoryRepo.findByColsEntityId("COL:ACC:1");
		assertEquals(0, updates.size());
	}

	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/cpeData.xml" })
	// Do not remove this DataSetExport - it's used as data set up in ColsOrderRepositoryIT - replacing the manually created xml where there
	// is no way of regenerating it in case something changes (as happened during refactoring - change in table names, class names etc)
	@DataSetExport(exportType = ExportType.PHYSICAL, tablesDataSet = "masterData/emptyDb_colsAll.xml")
	@DataSetConfig(resourceDir = "../esw-db/src/main/resources"
			, resourcePrefix = "testInputData")
	@TodoFixTest(value = "Check if resourcePrefix needs to be changed...", developer = Developer.Martin)
	public void canCreate_n20Confirm_setup() {

		OrderX preOrder = this.submitN20(false);

		// Submitting an N20 should have sent an 'in progress' notification
		this.assertion().cope().lastNotificationStati("1234", "in progress", "in progress", "preorder");

		// Get SubOrders we will make checks on
		OrderX s33PreOrderTibcoOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_S33_PRE_ORDER.getType());

		// Simulating the setting of the Cpe Model data
		setCpeDataWithOnePlug(preOrder, false);

		// Process the order to make sure it's processed without errors
		OrderDetails od = preOrder.process();
		Assert.assertFalse(od.isHasErrors());

		// S33 Preorder should be in progress, waiting for Tibco update of the data
		Assert.assertEquals(OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getState());

		// Simulate notification from SouthBound
		OrderType submittedOrder = this.pollLastRequest(OrderType.class);
		new LcsNotificationHandler().simulateNotification(submittedOrder);

		// Pre-Order should have gone to FINISH now, sending a preorder finish notification
		this.assertion().cope().lastNotificationStati("1234", "finished", "finished", "preorder");

		ColsAccessService colsAccessService = ServiceLocatorCdi.cdi(ColsAccessService.class);
		List<ColsAccess> accesses = colsAccessService.searchAll();
		// There should be no access created since this is the preordering
		assertEquals(0, accesses.size());
		// Make sure the order for the preorder is in progress now
		Assert.assertEquals(OrderState.IN_PROGRESS, preOrder.getState());

		this.submitN20Confirm();

		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel("FastEthernet0/1", "CpeModel-1", "HighEnd", false, false, null, null);
		CpeModelTestdataCreator.createCpeMaterial("HighEnd", "CpeModel-1");

		addOrderEvents();
		addErroneousOrders();
	}

	@Test
	public void canCreate_n20Confirm_CompletedWfmPortMoveNotification() throws ObjectNotFoundException {
		OrderX preOrder = this.submitN20(false);

		// Submitting an N20 should have sent an 'in progress' notification
		this.assertion().cope().lastNotificationStati("1234", "in progress", "in progress", "preorder");

		// Get SubOrders we will make checks on
		OrderX s33PreOrderTibcoOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_S33_PRE_ORDER.getType());

		// Simulating the setting of the Cpe Model data
		setCpeDataWithOnePlug(preOrder, false);

		// Process the order to make sure it's processed without errors
		OrderDetails od = preOrder.process();
		Assert.assertFalse(od.isHasErrors());

		// S33 Preorder should be in progress, waiting for Tibco update of the data
		Assert.assertEquals(OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getState());

		// Simulate notification from SouthBound
		OrderType submittedOrder = this.pollLastRequest(OrderType.class);
		new LcsNotificationHandler().simulateNotification(submittedOrder);

		// Pre-Order should have gone to FINISH now, sending a preorder finish notification
		this.assertion().cope().lastNotificationStati("1234", "finished", "finished", "preorder");

		ColsAccessService colsAccessService = ServiceLocatorCdi.cdi(ColsAccessService.class);
		List<ColsAccess> accesses = colsAccessService.searchAll();
		// There should be no access created since this is the preordering
		assertEquals(0, accesses.size());
		// Make sure the order for the preorder is in progress now
		Assert.assertEquals(OrderState.IN_PROGRESS, preOrder.getState());

		this.submitN20Confirm();

		// Should have sent an order / in progress notification. Notification needs to be sent with confirmation order ID
		this.assertion().cope().lastNotificationStati("1234-2", "in progress", "in progress", "order");

		OrderX n20ConfirmOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002", getModifiedSetter()));
		Assert.assertNotNull(n20ConfirmOrder);

		Assert.assertEquals(OrderState.IN_PROGRESS, n20ConfirmOrder.getSubOrderX(ColsOrderType.COLS_CPE_ORDER_DONE.getType()).getState());
		setCpeOrderDone(n20ConfirmOrder);

		Assert.assertEquals(OrderState.FINISHED, n20ConfirmOrder.getSubOrderX(ColsOrderType.COLS_CPE_ORDER_DONE.getType()).getState());

		// Simulate the GUI Assign of CPE Material. GUI calls same service as below:
		OrderX assignCpeMaterialOrder = n20ConfirmOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE_MATERIAL.getType());
		/** SET CPE MATERIALS **/
		setCpeMaterial(assignCpeMaterialOrder);

		setCreateColsAccessOrderTasks(n20ConfirmOrder, getMetaData());

		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrder = nbOrderService.findByOrderId("EOE-0000002");
		ColsProcessAccessConfirm process = (ColsProcessAccessConfirm) nbOrder.getColsProcess();

		// Simulate WFM Response
		int wfmId = Integer.valueOf(process.getResponse().getWfmId());
		ServiceLocatorCdi.cdi(ColsWfmSimulator.class).simulateResponse(wfmId, ColsWfmSimulator.CLOSED);
		// Get the ColsResponse from the WFM notification
		ColsResponse wfmColsResponse = this.getRequestHolder().pollRequest(ColsResponse.class);
		Boolean wfmEnabled = AttributeManagerUtil.getSystemPropertyBoolean("cols.wfm.ennabled");
		if (wfmEnabled) {
			Assert.assertTrue(wfmColsResponse.isSuccessful());
		}

		// check sent order
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_n20_copePreOrderConfirm.xml");

		// Main Order should stay in IN_PROGRESS
		Assert.assertEquals("main order should still be in IN_PROGRESS", OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getParentX()
				.getState());

		Assert.assertEquals("confirm order should be in process ", OrderState.IN_PROGRESS, n20ConfirmOrder.getState());

		// Simulate CoPE Notification
		new LcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		// Activate Alarm
		ServiceLocatorCdi.cdi(ColsOrderAlarmActivationService.class).activateAlarm(n20ConfirmOrder.getOrderId());

		// WFM response will be simulated automatically
		Assert.assertEquals("checking n20ConfirmOrder", OrderState.FINISHED, n20ConfirmOrder.getState());

		// We should receive a confirm notification for the confirmation
		this.assertLastNotificationXml("ColsServiceIT_n20_confirm_notification.xml");
		Assert.assertEquals("confirm order should be finished", OrderState.FINISHED, s33PreOrderTibcoOrder.getState());
		Assert.assertEquals("main order should be ", OrderState.FINISHED, s33PreOrderTibcoOrder.getParentX().getState());
		Assert.assertEquals("main order should be ", OrderState.FINISHED, preOrder.getState());

		accesses = colsAccessService.searchAll();
		// There should be one access created due to the confrimation
		assertEquals(1, accesses.size());
	}

	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml")
	public void canCreate_PortMovePhysical_L2Net() throws Exception {
		// This test test a Physical port move with invalid (due to master data) LanPortName in the request.
		// It tests the change of a defect LanPortName from "cpeLanPortName-1" to "cpeLanPortName-2".
		// Original request has "error-port" as new port name and therefor a LanPortName manual task should be triggered.
		// In this test the manual task action is triggered by the setCpeLanPortName(order, "cpeLanPortName-2") method.

		L2ServiceConnectivity connectivity = prepareColsAccessWithDualL2ServiceAccessAndConnectivity();

		// create an update Order to change cpeLanPportName from "cpeLanPortName-1" to "error-port"
		Order portMovePhysicalOrder = createPortMovePhysicalOrder_error_port();

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(portMovePhysicalOrder);

		// Get the order from the DB
		OrderX order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", this.getModifiedSetter()));
		Assert.assertNotNull(order);
		OrderX orderL2Net = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.002", this.getModifiedSetter()));
		OrderX orderConnectivity = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.003", this.getModifiedSetter()));
		OrderX orderAccess = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.004", this.getModifiedSetter()));

		// Check order state
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.WAITING, orderL2Net.getState());
		Assert.assertEquals(OrderState.WAITING, orderConnectivity.getState());
		Assert.assertEquals(OrderState.WAITING, orderAccess.getState());

		OrderDetails od = order.process();
		Assert.assertFalse(od.isHasErrors());

		// Check create ColsNorthboundOrder
		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrder = nbOrderService.findByOrderId("EOE-0000001");
		Assert.assertNotNull(nbOrder);
		Assert.assertEquals(4, nbOrder.getSuborders().size());

		// 3. Notify back the order status with orderUpdateStatus (cpeLanPportName = "error_port")
		this.assertLastNotificationXml("ColsServiceIT_update_portmovePhysicalCpe_notification_inProgress_error_port.xml");

		ColsRequestConnectivityPortMovePhysicalCpe aRequest = (ColsRequestConnectivityPortMovePhysicalCpe) order.getDataObjectNotNull(
				ColsProcessConnectivityPortMovePhysicalCpe.class, true)
				.getRequest();

		ColsResponseConnectivityPortMovePhysicalCpe aResponses = (ColsResponseConnectivityPortMovePhysicalCpe) order.getDataObjectNotNull(
				ColsProcessConnectivityPortMovePhysicalCpe.class, true)
				.getResponse();

		// Check the given CpeLanPortName value.
		assertEquals("error_port", aRequest.getCpeLanPortName());

		// CpeLanPortName should be null in Responses before user change or confirm the value.
		assertNull(aResponses.getCpeLanPortName());

		// 4. The user can CHANGE or CONFIRM the given CpeLanPortName.
		// Simulate the GUI Assign of CpeLanPortName.
		setCpeLanPortName(order, "cpeLanPortName-2", true);

		// CpeLanPortName should NOT be null in Responses after user change or confirm the value.
		aResponses = (ColsResponseConnectivityPortMovePhysicalCpe) order.getDataObjectNotNull(
				ColsProcessConnectivityPortMovePhysicalCpe.class, true)
				.getResponse();

		assertEquals("cpeLanPortName-2", aResponses.getCpeLanPortName());

		// 5. CoPE Order Process
		// The user's change of step 4 triggered the first order to process.
		// -----------------------------------------------------------------------------------------------------------
		// Check sent order
		// Sent order should contain new correct LanPortName: "cpeLanPortName-2"
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_update_l2Access_portmovePhysicalCpe_copeOrder.xml");

		// Make sure main order is not finished yet (first CoPE or should be IN_PROGRESS)
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, orderL2Net.getState());
		Assert.assertEquals(OrderState.FINISHED, orderConnectivity.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, orderAccess.getState());

		Assert.assertFalse(od.isHasErrors());

		// 5.1 CoPE: Simulate first notification
		// The notification finishes the first CoPE order and this triggers the second

		lcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		// Make sure the main order is not finished yet (first CoPE or should be FINISHED)
		Assert.assertEquals(OrderState.FINISHED, order.getState());
		Assert.assertEquals(OrderState.FINISHED, orderL2Net.getState());
		Assert.assertEquals(OrderState.FINISHED, orderConnectivity.getState());
		Assert.assertEquals(OrderState.FINISHED, orderAccess.getState());

		// 6. OE-COLS will send a Notification to CFU.
		// -----------------------------------------------------------------------------------------------------------
		// Notification should contain new correct LanPortName: "cpeLanPortName-2"
		this.assertLastNotificationXml("ColsServiceIT_update_portmovePhysicalCpe_notification.xml");

		connectivity = ServiceLocatorCdi.cdi(L2ServiceConnectivityService.class).searchAll().get(0);
		Assert.assertNotNull(connectivity);
		Assert.assertEquals("l2ServiceConnectivityPoId-1", connectivity.getPoId());
		Assert.assertEquals("cpeLanPortName-2", connectivity.getCpeLanPortName());

		ColsUpdateHistoryRepository updateHistoryRepo = ServiceLocatorCdi.cdi(ColsUpdateHistoryRepository.class,
				QualifierUtils.COLS_ANNO);
		List<ColsUpdateHistory> updates = updateHistoryRepo.findByColsEntityId(connectivity.getPoId());
		assertEquals(1, updates.size());
		SortedSet<AttributeUpdate> attributes = updates.get(0).getAttributes();
		assertEquals(1, attributes.size());
		assertEquals("l2ServiceConnectivityPoId-1", updates.get(0).getColsEntityId());
		assertEquals("UnitTest", updates.get(0).getChangeUser());
		assertEquals("CpeLanPortName", attributes.first().getAttributeName());
		assertEquals("cpeLanPortName-1", attributes.first().getOldValue());
		assertEquals("cpeLanPortName-2", attributes.first().getNewValue());
	}

	@Test
	@TodoFixTest(value = "The Cpe_Cpelan relation switches back from set to unset - seems to be some problem still... maybe it helps when the remote interfaces are gone...", developer = Developer.Martin)
	public void canCreate_PortMovePhysical_L2Net_and_P2PNet() throws Exception {

		// We suppose the request has a correct cpeLanPortName
		String oldLanPortName = "cpeLanPortName-1";
		String newLanPortName = "cpeLanPortName-2";
		String otherLanPortName = "cpeLanPortName-3";

		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel(oldLanPortName, "cpeModel-1", "cpeOption-1", false, false, null, null);
		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel(newLanPortName, "cpeModel-1", "cpeOption-1", false, false, null, null);
		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel(otherLanPortName, "cpeModel-1", "cpeOption-1", false, false, null, null);

		/*
		 * This test creates a ColsAccess that connects to 2 Nets over 2 Connectivities.
		 * First is for L2Net and second is for P2PNet.
		 * Both are SINGLE-HOMED.
		 * This means that both ServiceConnectivities connect to 1 ServiceAccess each.
		 * ColsAccess
		 * |
		 * +-----L2Connectivity
		 * | |
		 * | +-----L2Net
		 * | |
		 * | +-----ServiceAccess
		 * |
		 * +-----L3Connectivity
		 * |
		 * +-----P2PNet
		 * |
		 * +-----ServiceAccess2
		 */

		Object[] con = canCreate_PortMovePhysical_L2Net_and_P2PNet_prepareDB(oldLanPortName);
		L2ServiceConnectivity l2Connectivity = (L2ServiceConnectivity) con[0];
		L3ServiceConnectivity l3Connectivity = (L3ServiceConnectivity) con[1];

		// Assert to check existing values
		assertEquals("l2ServiceConnectivityPoId-1", l2Connectivity.getPoId());
		assertEquals("ServiceIdL2ServiceAccess", l2Connectivity.getServiceAccessCfsId1());
		assertEquals("l2ServiceAccessPoId-1", l2Connectivity.getL2ServiceAccessId());
		assertEquals(oldLanPortName, l2Connectivity.getCpeLanPortName());
		assertEquals("l2ServiceAccessPoId-1", l2Connectivity.getL2ServiceAccess().getPoId());
		// assertEquals("cpeLanPortName", l2Connectivity.getL2ServiceConnectivityAccess().getColsAccess().getLogicalPhysicalCpe().getCpe()
		// .getCpeLanPortName());

		assertEquals("l3ServiceConnectivityPoId-1", l3Connectivity.getPoId());
		assertEquals("ServiceIdL3ServiceAccess", l3Connectivity.getServiceAccessCfsId1());
		assertEquals("l3ServiceAccessPoId-1", l3Connectivity.getL3ServiceAccessId());
		assertEquals(oldLanPortName, l3Connectivity.getCpeLanPortName());
		assertEquals("l3ServiceAccessPoId-1", l3Connectivity.getL3ServiceConnectivityAccess().getPoId());
		// assertEquals("cpeLanPortName", l3Connectivity.getL3ServiceConnectivityAccess().getColsAccess().getLogicalPhysicalCpe().getCpe()
		// .getCpeLanPortName());

		// 2. Create the COLS Orders
		// -----------------------------------------------------------------------------------------------------------
		// create an update Order to change cpeLanPportName to "FastEthernet0/2"
		Order portMovePhysicalOrder = createPortMovePhysicalOrder();

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(portMovePhysicalOrder);

		// Get the order from the DB
		OrderX order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", this.getModifiedSetter()));
		Assert.assertNotNull(order);
		OrderX orderL2Net = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.002", this.getModifiedSetter()));
		OrderX orderConnectivity = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.003", this.getModifiedSetter()));
		OrderX orderAccess = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.004", this.getModifiedSetter()));

		// Check order state
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, orderL2Net.getState());
		Assert.assertEquals(OrderState.FINISHED, orderConnectivity.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, orderAccess.getState());

		OrderDetails od = order.process();
		Assert.assertFalse(od.isHasErrors());

		// 3. OE Notified back the order status
		// -----------------------------------------------------------------------------------------------------------
		this.assertLastNotificationXml("ColsServiceIT_update_portmovePhysicalCpe_notification_inProgress.xml");

		ColsRequestConnectivityPortMovePhysicalCpe aRequest = (ColsRequestConnectivityPortMovePhysicalCpe) order.getDataObjectNotNull(
				ColsProcessConnectivityPortMovePhysicalCpe.class, true)
				.getRequest();

		// Check the CpeLanPortName value on Request.
		assertEquals(newLanPortName, aRequest.getCpeLanPortName());

		/*
		 * We assume that the request is already good - no need to ask for CpeLanPortName
		 * // 4. The user CHANGEs or CONFIRMs the given CpeLanPortName.
		 * // -----------------------------------------------------------------------------------------------------------
		 * // Simulate the GUI Assign of CpeLanPortName.
		 * setCpeLanPortName(order, "FastEthernet0/2");
		 * // CpeLanPortName should NOT be null in Responses after user change or confirm the value.
		 * aResponses = (ColsResponseConnectivityPortMovePhysicalCpe)
		 * order.getDataObjectNotNull(ColsProcessConnectivityPortMovePhysicalCpe.class,
		 * true)
		 * .getResponse();
		 * assertEquals("FastEthernet0/2", aResponses.getCpeLanPortName());
		 */

		// 5. CoPE Order Process
		// The user's change of step 4 triggered the first order to process.
		// -----------------------------------------------------------------------------------------------------------
		// Check sent order
		this.assertion().cope()
				.lastSentOrderEqualsResource("ColsServiceIT_update_Access_portmovePhysicalCpe_L2Net_and_P2PNet_copeOrder.xml");

		// Make sure the main order is not finished yet (first CoPE order should be IN_PROGRESS)
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, orderL2Net.getState());
		Assert.assertEquals(OrderState.FINISHED, orderConnectivity.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, orderAccess.getState());

		Assert.assertFalse(od.isHasErrors());

		// 5.1 CoPE: Simulate first notification
		// The notification finishes the first CoPE order that will in turn trigger the second

		lcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		this.assertion().cope()
				.lastSentOrderEqualsResource("ColsServiceIT_update_Access_portmovePhysicalCpe_L2Net_and_P2PNet_2_copeOrder.xml");

		// Make sure the main order is not finished yet (first CoPE order should be FINISHED)
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, orderL2Net.getState());
		Assert.assertEquals(OrderState.FINISHED, orderConnectivity.getState());
		Assert.assertEquals(OrderState.FINISHED, orderAccess.getState());

		Assert.assertFalse(od.isHasErrors());

		OrderX orderP2PNet = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.005", this.getModifiedSetter()));
		orderConnectivity = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.006", this.getModifiedSetter()));
		orderAccess = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.007", this.getModifiedSetter()));

		// Make sure the main order is not finished yet (second CoPE order should be IN_PROGRESS)
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, orderP2PNet.getState());
		Assert.assertEquals(OrderState.FINISHED, orderConnectivity.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, orderAccess.getState());

		// 5.2 CoPE: Simulate second notification
		// The notification finishes the second CoPE order

		lcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		Assert.assertFalse(od.isHasErrors());

		// 6. OE-COLS will send a Notification to CFU (triggered by finished main order)
		// -----------------------------------------------------------------------------------------------------------
		this.assertLastNotificationXml("ColsServiceIT_update_portmovePhysicalCpe_notification.xml");

		// Make sure the second CoPE order and main order are finished now
		Assert.assertEquals(OrderState.FINISHED, orderAccess.getState());
		Assert.assertEquals(OrderState.FINISHED, order.getState());

		// -----------------------------------------------------------------------------------------------------------
		// for connectivity: check if the updates were actually updated on the installed base
		// as well after the notification was received
		l2Connectivity = ServiceLocatorCdi.cdi(L2ServiceConnectivityService.class).searchAll().get(0);
		Assert.assertEquals("l2ServiceConnectivityPoId-1", l2Connectivity.getPoId());

		assertNotNull(l2Connectivity.getL2ServiceAccess());
		Assert.assertEquals(newLanPortName, l2Connectivity.getCpeLanPortName());
		// -----------------------------------------------------------------------------------------------------------
		// for connectivity2: check if the updates were actually updated on the installed base
		// as well after the notification was received
		l3Connectivity = ServiceLocatorCdi.cdi(L3ServiceConnectivityService.class).searchAll().get(0);
		Assert.assertEquals("l3ServiceConnectivityPoId-1", l3Connectivity.getPoId());

		assertNotNull(l3Connectivity.getL3ServiceConnectivityAccess());
		Assert.assertEquals(newLanPortName, l3Connectivity.getCpeLanPortName());
		// -----------------------------------------------------------------------------------------------------------

		ColsUpdateHistoryRepository updateHistoryRepo = ServiceLocatorCdi.cdi(ColsUpdateHistoryRepository.class, QualifierUtils.COLS_ANNO);
		List<ColsUpdateHistory> updates = updateHistoryRepo.findByColsEntityId(l3Connectivity.getPoId());
		assertEquals(1, updates.size());
		SortedSet<AttributeUpdate> attributes = updates.get(0).getAttributes();
		assertEquals(1, attributes.size());
		assertEquals("l3ServiceConnectivityPoId-1", updates.get(0).getColsEntityId());
		assertEquals("OH-CIH", updates.get(0).getChangeUser());
		assertEquals("CpeLanPortName", attributes.first().getAttributeName());
		assertEquals("cpeLanPortName-1", attributes.first().getOldValue());
		assertEquals("cpeLanPortName-2", attributes.first().getNewValue());
	}

	@Test
	public void canCreate_PortMovePhysical_L2Net_twice() throws Exception {
		String oldLanPortName = "cpeLanPortName-1";
		String newLanPortName = "cpeLanPortName-2";
		String otherLanPortName = "cpeLanPortName-3";

		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel(oldLanPortName, "cpeModel-1", "cpeOption-1", false, false, null, null);
		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel(newLanPortName, "cpeModel-1", "cpeOption-1", false, false, null, null);
		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel(otherLanPortName, "cpeModel-1", "cpeOption-1", false, false, null, null);

		/*
		 * This test creates a ColsAccess that connects to 2 L2Nets over 2 Connectivities (L2ServiceConnectivity).
		 * Both are SINGLE-HOMED.
		 * ColsAccess
		 * |
		 * +-----Connectivity (DUAL-HOMED)
		 * | |
		 * | +-----L2Net
		 * | |
		 * | +-----ServiceAccess
		 * |
		 * +-----Connectivity2 (SINGLE-HOMED)
		 * |
		 * +-----L2Net2
		 * |
		 * +-----ServiceAccess2
		 */

		L2ServiceConnectivityService connectivityService = ServiceLocatorCdi.cdi(L2ServiceConnectivityService.class);

		// 1. Insert the required test data in Installed Base
		// -----------------------------------------------------------------------------------------------------------
		// create a connectivity -> which needs an L2Net and a ColsAccess

		ColsAccess colsAccess = this.testData().cols().newColsAccess("1", ColsAccessLegType.HIGH_END);
		L2Net l2Net = this.testData().cols().newL2Net("1", "CES");
		L2ServiceConnectivity connectivity = this.testData().cols().newL2ServiceConnectivity("1", colsAccess, l2Net);

		Assert.assertNotNull(colsAccess);
		Assert.assertNotNull(l2Net);
		Assert.assertNotNull(connectivity);
		Assert.assertEquals(1, connectivityService.searchAll().size());

		// set some values into the connectivity attributes that's about to be updated through the order

		connectivity.setCpeLanPortName(oldLanPortName);
		connectivity.setServiceAccessCfsId1("ServiceIdL2ServiceAccess");

		connectivity.getL2ServiceAccess().getColsAccess().getLogicalPhysicalCpe().getCpe().setCpeLanPortName(oldLanPortName);

		connectivity = connectivityService.update(connectivity);

		// Assert to check existing values
		Assert.assertEquals("l2ServiceConnectivityPoId-1", connectivity.getPoId());
		Assert.assertEquals("ServiceIdL2ServiceAccess", connectivity.getServiceAccessCfsId1());
		Assert.assertEquals("l2ServiceAccessPoId-1", connectivity.getL2ServiceAccessId());
		Assert.assertEquals(oldLanPortName, connectivity.getCpeLanPortName());

		assertEquals("l2ServiceAccessPoId-1", connectivity.getL2ServiceAccess().getPoId());

		// -----------------------------------------------------------------------------------------------------------
		// create a connectivity2

		L2Net l2Net2 = this.testData().cols().newL2Net("2", "CES");
		L2ServiceConnectivity connectivity2 = this.testData().cols().newL2ServiceConnectivity("2", colsAccess, l2Net2);

		Assert.assertNotNull(l2Net2);
		Assert.assertNotNull(connectivity2);
		Assert.assertEquals(2, connectivityService.searchAll().size());

		// set some values into the connectivity attributes that's about to be updated through the order
		connectivity2.setCpeLanPortName(oldLanPortName);
		connectivity2.setServiceAccessCfsId1("ServiceIdL2ServiceAccess2");
		connectivity2.getL2ServiceAccess().getColsAccess().getLogicalPhysicalCpe().getCpe().setCpeLanPortName(oldLanPortName);

		connectivity2 = connectivityService.update(connectivity2);

		// Assert to check existing values
		assertEquals("l2ServiceConnectivityPoId-2", connectivity2.getPoId());
		assertEquals("ServiceIdL2ServiceAccess2", connectivity2.getServiceAccessCfsId1());
		assertEquals("l2ServiceAccessPoId-2", connectivity2.getL2ServiceAccessId());
		assertEquals(oldLanPortName, connectivity2.getCpeLanPortName());

		assertEquals("l2ServiceAccessPoId-2", connectivity2.getL2ServiceAccess().getPoId());

		// 2. Create the COLS Orders
		// -----------------------------------------------------------------------------------------------------------
		// create an update Order to change cpeLanPportName from "cpeLanPortName-1" to "cpeLanPortName-2"
		Order portMovePhysicalOrder = createPortMovePhysicalOrder();

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(portMovePhysicalOrder);

		// Get the order from the DB
		OrderX order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", this.getModifiedSetter()));
		Assert.assertNotNull(order);
		OrderX orderL2Net = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.002", this.getModifiedSetter()));
		OrderX orderConnectivity = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.003", this.getModifiedSetter()));
		OrderX orderAccess = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.004", this.getModifiedSetter()));

		// Check order state
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, orderL2Net.getState());
		Assert.assertEquals(OrderState.FINISHED, orderConnectivity.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, orderAccess.getState());

		OrderDetails od = order.process();
		Assert.assertFalse(od.isHasErrors());

		// 3. OE Notified back the order status
		// -----------------------------------------------------------------------------------------------------------
		this.assertLastNotificationXml("ColsServiceIT_update_portmovePhysicalCpe_notification_inProgress.xml");

		ColsRequestConnectivityPortMovePhysicalCpe aRequest = (ColsRequestConnectivityPortMovePhysicalCpe) order.getDataObjectNotNull(
				ColsProcessConnectivityPortMovePhysicalCpe.class, true)
				.getRequest();

		// Check the CpeLanPortName value on Request.
		assertEquals(newLanPortName, aRequest.getCpeLanPortName());

		// 4. CpeLanPortName is already correct, no need to ask

		// 5. CoPE Order Process
		// The user's change of step 4 triggered the first order to process.
		// -----------------------------------------------------------------------------------------------------------
		// Check sent order
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_update_l2Access_portmovePhysicalCpe_twice_copeOrder.xml");

		// Make sure main order is not finished yet (first CoPE or should be IN_PROGRESS)
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, orderL2Net.getState());
		Assert.assertEquals(OrderState.FINISHED, orderConnectivity.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, orderAccess.getState());

		Assert.assertFalse(od.isHasErrors());

		// 5.1 CoPE: Simulate first notification
		// The notification finishes the first CoPE order and this triggers the second

		lcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_update_l2Access_portmovePhysicalCpe_twice2_copeOrder.xml");

		// Make sure the main order is not finished yet (first CoPE or should be FINISHED)
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, orderL2Net.getState());
		Assert.assertEquals(OrderState.FINISHED, orderConnectivity.getState());
		Assert.assertEquals(OrderState.FINISHED, orderAccess.getState());

		Assert.assertFalse(od.isHasErrors());

		orderL2Net = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.005", this.getModifiedSetter()));
		orderConnectivity = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.006", this.getModifiedSetter()));
		orderAccess = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.007", this.getModifiedSetter()));

		// Make sure the main order is not finished yet (second CoPE or should be IN_PROGRESS)
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, orderL2Net.getState());
		Assert.assertEquals(OrderState.FINISHED, orderConnectivity.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, orderAccess.getState());

		// 5.1 CoPE: Simulate second notification

		lcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		// 6. OE-COLS will send a Notification to CFU.
		// -----------------------------------------------------------------------------------------------------------
		this.assertLastNotificationXml("ColsServiceIT_update_portmovePhysicalCpe_notification.xml");

		// Make sure the main order and third CoPE orders are FINISHED)
		Assert.assertEquals(OrderState.FINISHED, orderAccess.getState());
		Assert.assertEquals(OrderState.FINISHED, order.getState());

		// -----------------------------------------------------------------------------------------------------------
		// for connectivity: check if the updates were actually updated on the installed base
		// as well after the notification was received
		connectivity = ServiceLocatorCdi.cdi(L2ServiceConnectivityService.class).searchAll().get(0);
		Assert.assertEquals("l2ServiceConnectivityPoId-1", connectivity.getPoId());

		assertNotNull(connectivity.getL2ServiceAccess());
		Assert.assertEquals(newLanPortName, connectivity.getCpeLanPortName());
		// -----------------------------------------------------------------------------------------------------------
		// for connectivity2: check if the updates were actually updated on the installed base
		// as well after the notification was received
		connectivity2 = ServiceLocatorCdi.cdi(L2ServiceConnectivityService.class).searchAll().get(1);
		Assert.assertEquals("l2ServiceConnectivityPoId-2", connectivity2.getPoId());

		assertNotNull(connectivity2.getL2ServiceAccess());
		Assert.assertEquals(newLanPortName, connectivity2.getCpeLanPortName());
		// -----------------------------------------------------------------------------------------------------------

		// Connectivity 1
		ColsUpdateHistoryRepository updateHistoryRepo = ServiceLocatorCdi.cdi(ColsUpdateHistoryRepository.class, QualifierUtils.COLS_ANNO);
		List<ColsUpdateHistory> updates = updateHistoryRepo.findByColsEntityId(connectivity.getPoId());
		assertEquals(1, updates.size());
		SortedSet<AttributeUpdate> attributes = updates.get(0).getAttributes();
		assertEquals(1, attributes.size());
		assertEquals("l2ServiceConnectivityPoId-1", updates.get(0).getColsEntityId());
		assertEquals("OH-CIH", updates.get(0).getChangeUser());
		assertEquals("CpeLanPortName", attributes.first().getAttributeName());
		assertEquals("cpeLanPortName-1", attributes.first().getOldValue());
		assertEquals("cpeLanPortName-2", attributes.first().getNewValue());

		// Connectivity 2
		updates = updateHistoryRepo.findByColsEntityId(connectivity2.getPoId());
		assertEquals(1, updates.size());
		attributes = updates.get(0).getAttributes();
		assertEquals(1, attributes.size());
		assertEquals("l2ServiceConnectivityPoId-2", updates.get(0).getColsEntityId());
		assertEquals("OH-CIH", updates.get(0).getChangeUser());
		assertEquals("CpeLanPortName", attributes.first().getAttributeName());
		assertEquals("cpeLanPortName-1", attributes.first().getOldValue());
		assertEquals("cpeLanPortName-2", attributes.first().getNewValue());
	}

	@Test
	public void canCreate_PortMovePhysical_L2Net_dual() throws Exception {
		String oldLanPortName = "cpeLanPortName-1";
		String newLanPortName = "cpeLanPortName-2";
		String otherLanPortName = "cpeLanPortName-3";

		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel(oldLanPortName, "cpeModel-1", "cpeOption-1", false, false, null, null);
		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel(newLanPortName, "cpeModel-1", "cpeOption-1", false, false, null, null);
		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel(otherLanPortName, "cpeModel-1", "cpeOption-1", false, false, null, null);

		/*
		 * This test is for a ColsAccess that connects to one L2Net over 2 Connectivities (L2ServiceConnectivity).
		 * This means that the L2ServiceConnectivity connects to 2 ServiceAccesses (L2ServiceConnectivityAccesses).
		 * ColsAccess
		 * |
		 * +-----Connectivity (DUAL-HOMED)
		 * |
		 * +-----L2Net
		 * |
		 * +-----ServiceAccess
		 * +-----ServiceAccess2
		 */

		L2ServiceConnectivityService connectivityService = ServiceLocatorCdi.cdi(L2ServiceConnectivityService.class);

		// 1. Insert the required test data in Installed Base
		// -----------------------------------------------------------------------------------------------------------
		// create a connectivity -> which needs an L2Net and a ColsAccess

		ColsAccess colsAccess = this.testData().cols().newColsAccess("1", ColsAccessLegType.HIGH_END);
		L2Net l2Net = this.testData().cols().newL2Net("1", "CES");
		L2ServiceConnectivity connectivity = this.testData().cols().newDualHomedOneCaL2ServiceConnectivity("1", colsAccess, l2Net);

		assertNotNull(colsAccess);
		assertNotNull(l2Net);
		assertNotNull(connectivity);
		assertEquals(1, connectivityService.searchAll().size());

		// set some values into the connectivity attributes that's about to be updated through the order

		connectivity.setCpeLanPortName(oldLanPortName);
		connectivity.setCpeLanPortName2(oldLanPortName);
		connectivity.setServiceAccessCfsId1("ServiceIdL2ServiceAccess");
		connectivity.setServiceAccessCfsId2("ServiceIdL2ServiceAccess2");

		connectivity.getL2ServiceAccess().getColsAccess().getLogicalPhysicalCpe().getCpe().setCpeLanPortName(oldLanPortName);

		connectivity = connectivityService.update(connectivity);

		// Assert to check existing values
		assertEquals("l2ServiceConnectivityPoId-1", connectivity.getPoId());
		assertEquals("ServiceIdL2ServiceAccess", connectivity.getServiceAccessCfsId1());
		assertEquals("ServiceIdL2ServiceAccess2", connectivity.getServiceAccessCfsId2());
		assertEquals("l2ServiceAccessPoId-1", connectivity.getL2ServiceAccessId());

		assertEquals("l2ServiceAccessPoId-1", connectivity.getL2ServiceAccess().getPoId());

		// 2. Create the COLS Orders
		// -----------------------------------------------------------------------------------------------------------
		// create an update Order to change cpeLanPportName from "cpeLanPortName-1" to "cpeLanPortName-2"
		Order portMovePhysicalOrder = createPortMovePhysicalOrder();

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(portMovePhysicalOrder);

		// Get the order from the DB
		OrderX order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", this.getModifiedSetter()));
		Assert.assertNotNull(order);
		OrderX orderL2Net = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.002", this.getModifiedSetter()));
		OrderX orderConnectivity = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.003", this.getModifiedSetter()));
		OrderX orderAccess = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.004", this.getModifiedSetter()));
	}

	@Test
	public void canCreate_PortMovePhysical_P2PNet_twice() throws Exception {
		String oldLanPortName = "cpeLanPortName-1";
		String newLanPortName = "cpeLanPortName-2";
		String otherLanPortName = "cpeLanPortName-3";

		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel(oldLanPortName, "cpeModel-1", "cpeOption-1", false, false, null, null);
		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel(newLanPortName, "cpeModel-1", "cpeOption-1", false, false, null, null);
		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel(otherLanPortName, "cpeModel-1", "cpeOption-1", false, false, null, null);

		/*
		 * This test creates a ColsAccess that connects to 2 P2PNets over 2 Connectivities (L3ServiceConnectivity).
		 * Both are SINGLE-HOMED.
		 * ColsAccess
		 * |
		 * +-----Connectivity (SINGLE-HOMED)
		 * | |
		 * | +-----P2PNet
		 * | |
		 * | +-----ServiceAccess
		 * |
		 * +-----Connectivity2 (SINGLE-HOMED)
		 * |
		 * +-----P2PNet2
		 * |
		 * +-----ServiceAccess2
		 */

		L3ServiceConnectivityService connectivityService = ServiceLocatorCdi.cdi(L3ServiceConnectivityService.class);

		// 1. Insert the required test data in Installed Base
		// -----------------------------------------------------------------------------------------------------------
		// create a connectivity -> which needs an P2PNet and a ColsAccess

		ColsAccess colsAccess = this.testData().cols().newColsAccess("1", ColsAccessLegType.HIGH_END);
		P2PNet l3Net = this.testData().cols().newP2PNet("1");
		L3ServiceConnectivity connectivity = this.testData().cols().newL3ServiceConnectivity("1", colsAccess, l3Net);

		Assert.assertNotNull(colsAccess);
		Assert.assertNotNull(l3Net);
		Assert.assertNotNull(connectivity);
		Assert.assertEquals(1, connectivityService.searchAll().size());

		// set some values into the connectivity attributes that's about to be updated through the order

		connectivity.setCpeLanPortName(oldLanPortName);
		connectivity.setServiceAccessCfsId1("ServiceIdL3ServiceAccess");

		connectivity.getL3ServiceConnectivityAccess().getColsAccess().getLogicalPhysicalCpe().getCpe().setCpeLanPortName(oldLanPortName);

		connectivity = connectivityService.update(connectivity);

		// Assert to check existing values
		Assert.assertEquals("l3ServiceConnectivityPoId-1", connectivity.getPoId());
		Assert.assertEquals("ServiceIdL3ServiceAccess", connectivity.getServiceAccessCfsId1());
		Assert.assertEquals("l3ServiceAccessPoId-1", connectivity.getL3ServiceAccessId());
		Assert.assertEquals(oldLanPortName, connectivity.getCpeLanPortName());

		assertEquals("l3ServiceAccessPoId-1", connectivity.getL3ServiceConnectivityAccess().getPoId());

		// -----------------------------------------------------------------------------------------------------------
		// create a connectivity2

		P2PNet l3Net2 = this.testData().cols().newP2PNet("2");
		L3ServiceConnectivity connectivity2 = this.testData().cols().newL3ServiceConnectivity("2", colsAccess, l3Net2);

		Assert.assertNotNull(l3Net2);
		Assert.assertNotNull(connectivity2);
		Assert.assertEquals(2, connectivityService.searchAll().size());

		// set some values into the connectivity attributes that's about to be updated through the order
		connectivity2.setCpeLanPortName(oldLanPortName);
		connectivity2.setServiceAccessCfsId1("ServiceIdL3ServiceAccess2");
		connectivity2.getL3ServiceConnectivityAccess().getColsAccess().getLogicalPhysicalCpe().getCpe().setCpeLanPortName(oldLanPortName);

		connectivity2 = connectivityService.update(connectivity2);

		// Assert to check existing values
		assertEquals("l3ServiceConnectivityPoId-2", connectivity2.getPoId());
		assertEquals("ServiceIdL3ServiceAccess2", connectivity2.getServiceAccessCfsId1());
		assertEquals("l3ServiceAccessPoId-2", connectivity2.getL3ServiceAccessId());
		assertEquals(oldLanPortName, connectivity2.getCpeLanPortName());

		assertEquals("l3ServiceAccessPoId-2", connectivity2.getL3ServiceConnectivityAccess().getPoId());

		// 2. Create the COLS Orders
		// -----------------------------------------------------------------------------------------------------------
		// create an update Order to change cpeLanPportName from oldLanPortName to newLanPortName
		Order portMovePhysicalOrder = createPortMovePhysicalOrder();

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(portMovePhysicalOrder);

		// Get the order from the DB
		OrderX order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", this.getModifiedSetter()));
		Assert.assertNotNull(order);
		OrderX orderP2PNet = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.002", this.getModifiedSetter()));
		OrderX orderConnectivity = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.003", this.getModifiedSetter()));
		OrderX orderAccess = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.004", this.getModifiedSetter()));

		// Check order state
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, orderP2PNet.getState());
		Assert.assertEquals(OrderState.FINISHED, orderConnectivity.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, orderAccess.getState());

		OrderDetails od = order.process();
		Assert.assertFalse(od.isHasErrors());

		// 3. OE Notified back the order status
		// -----------------------------------------------------------------------------------------------------------
		this.assertLastNotificationXml("ColsServiceIT_update_portmovePhysicalCpe_notification_inProgress.xml");

		ColsRequestConnectivityPortMovePhysicalCpe aRequest = (ColsRequestConnectivityPortMovePhysicalCpe) order.getDataObjectNotNull(
				ColsProcessConnectivityPortMovePhysicalCpe.class, true)
				.getRequest();

		ColsResponseConnectivityPortMovePhysicalCpe aResponses = (ColsResponseConnectivityPortMovePhysicalCpe) order.getDataObjectNotNull(
				ColsProcessConnectivityPortMovePhysicalCpe.class, true)
				.getResponse();

		// Check the CpeLanPortName value on Request.
		assertEquals(newLanPortName, aRequest.getCpeLanPortName());

		// 4. CpeLanPortName is already ok so no need to ask
		// -----------------------------------------------------------------------------------------------------------
		assertEquals(newLanPortName, aResponses.getCpeLanPortName());

		// 5. CoPE Order Process
		// The user's change of step 4 triggered the first order to process.
		// -----------------------------------------------------------------------------------------------------------
		// Check sent order
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_update_l3Access_portmovePhysicalCpe_twice_copeOrder.xml");

		// Make sure main order is not finished yet (first CoPE or should be IN_PROGRESS)
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, orderP2PNet.getState());
		Assert.assertEquals(OrderState.FINISHED, orderConnectivity.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, orderAccess.getState());

		Assert.assertFalse(od.isHasErrors());

		// 5.1 CoPE: Simulate first notification
		// The notification finishes the first CoPE order and this triggers the second

		lcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_update_l3Access_portmovePhysicalCpe_twice2_copeOrder.xml");

		// Make sure the main order is not finished yet (first CoPE or should be FINISHED)
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, orderP2PNet.getState());
		Assert.assertEquals(OrderState.FINISHED, orderConnectivity.getState());
		Assert.assertEquals(OrderState.FINISHED, orderAccess.getState());

		Assert.assertFalse(od.isHasErrors());

		orderP2PNet = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.005", this.getModifiedSetter()));
		orderConnectivity = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.006", this.getModifiedSetter()));
		orderAccess = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.007", this.getModifiedSetter()));

		// Make sure the main order is not finished yet (second CoPE or should be IN_PROGRESS)
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, orderP2PNet.getState());
		Assert.assertEquals(OrderState.FINISHED, orderConnectivity.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, orderAccess.getState());

		// 5.1 CoPE: Simulate second notification

		lcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		// 6. OE-COLS will send a Notification to CFU.
		// -----------------------------------------------------------------------------------------------------------
		this.assertLastNotificationXml("ColsServiceIT_update_portmovePhysicalCpe_notification.xml");

		// Make sure the main order and third CoPE orders are FINISHED)
		Assert.assertEquals(OrderState.FINISHED, orderAccess.getState());
		Assert.assertEquals(OrderState.FINISHED, order.getState());

		// -----------------------------------------------------------------------------------------------------------
		// for connectivity: check if the updates were actually updated on the installed base
		// as well after the notification was received
		connectivity = ServiceLocatorCdi.cdi(L3ServiceConnectivityService.class).searchAll().get(0);
		Assert.assertEquals("l3ServiceConnectivityPoId-1", connectivity.getPoId());

		assertNotNull(connectivity.getL3ServiceConnectivityAccess());
		Assert.assertEquals(newLanPortName, connectivity.getCpeLanPortName());
		// -----------------------------------------------------------------------------------------------------------
		// for connectivity2: check if the updates were actually updated on the installed base
		// as well after the notification was received
		connectivity2 = ServiceLocatorCdi.cdi(L3ServiceConnectivityService.class).searchAll().get(1);
		Assert.assertEquals("l3ServiceConnectivityPoId-2", connectivity2.getPoId());

		assertNotNull(connectivity2.getL3ServiceConnectivityAccess());
		Assert.assertEquals(newLanPortName, connectivity2.getCpeLanPortName());
		// -----------------------------------------------------------------------------------------------------------

		ColsUpdateHistoryRepository updateHistoryRepo = ServiceLocatorCdi.cdi(ColsUpdateHistoryRepository.class, QualifierUtils.COLS_ANNO);
		List<ColsUpdateHistory> updates = updateHistoryRepo.findByColsEntityId(connectivity.getPoId());
		assertEquals(1, updates.size());
		SortedSet<AttributeUpdate> attributes = updates.get(0).getAttributes();
		assertEquals(1, attributes.size());
		assertEquals("l3ServiceConnectivityPoId-1", updates.get(0).getColsEntityId());
		assertEquals("OH-CIH", updates.get(0).getChangeUser());
		assertEquals("CpeLanPortName", attributes.first().getAttributeName());
		assertEquals("cpeLanPortName-1", attributes.first().getOldValue());
		assertEquals("cpeLanPortName-2", attributes.first().getNewValue());

		updates = updateHistoryRepo.findByColsEntityId(connectivity2.getPoId());
		assertEquals(1, updates.size());
		attributes = updates.get(0).getAttributes();
		assertEquals(1, attributes.size());
		assertEquals("l3ServiceConnectivityPoId-2", updates.get(0).getColsEntityId());
		assertEquals("OH-CIH", updates.get(0).getChangeUser());
		assertEquals("CpeLanPortName", attributes.first().getAttributeName());
		assertEquals("cpeLanPortName-1", attributes.first().getOldValue());
		assertEquals("cpeLanPortName-2", attributes.first().getNewValue());
	}

	@Test
	public void canCreate_PortMovePhysical_P2PNet_dual() throws Exception {
		String oldLanPortName = "cpeLanPortName-1";
		String newLanPortName = "cpeLanPortName-2";
		String otherLanPortName = "cpeLanPortName-3";

		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel(oldLanPortName, "cpeModel-1", "cpeOption-1", false, false, null, null);
		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel(newLanPortName, "cpeModel-1", "cpeOption-1", false, false, null, null);
		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel(otherLanPortName, "cpeModel-1", "cpeOption-1", false, false, null, null);

		/*
		 * This test is for a ColsAccess that connects to one P2PNet over 2 Connectivities (L3ServiceConnectivity).
		 * This means that the L3ServiceConnectivity connects to 2 ServiceAccesses (L3ServiceConnectivityAccesses).
		 * ColsAccess
		 * |
		 * +-----Connectivity (DUAL-HOMED)
		 * |
		 * +-----P2PNet
		 * |
		 * +-----ServiceAccess
		 * +-----ServiceAccess2
		 */

		L3ServiceConnectivityService connectivityService = ServiceLocatorCdi.cdi(L3ServiceConnectivityService.class);

		// 1. Insert the required test data in Installed Base
		// -----------------------------------------------------------------------------------------------------------
		// create a connectivity -> which needs an P2PNet and a ColsAccess

		ColsAccess colsAccess = this.testData().cols().newColsAccess("1", ColsAccessLegType.HIGH_END);
		P2PNet l3Net = this.testData().cols().newP2PNet("1");
		L3ServiceConnectivity connectivity = this.testData().cols().newDualHomedOneCaL3ServiceConnectivity("1", colsAccess, l3Net);

		assertNotNull(colsAccess);
		assertNotNull(l3Net);
		assertNotNull(connectivity);
		assertEquals(1, connectivityService.searchAll().size());

		// set some values into the connectivity attributes that's about to be updated through the order

		connectivity.setCpeLanPortName(oldLanPortName);
		connectivity.setCpeLanPortName2(oldLanPortName);
		connectivity.setServiceAccessCfsId1("ServiceIdL3ServiceAccess");
		connectivity.setServiceAccessCfsId2("ServiceIdL3ServiceAccess2");

		connectivity.getL3ServiceConnectivityAccess().getColsAccess().getLogicalPhysicalCpe().getCpe().setCpeLanPortName(oldLanPortName);

		connectivity = connectivityService.update(connectivity);

		// Assert to check existing values
		assertEquals("l3ServiceConnectivityPoId-1", connectivity.getPoId());
		assertEquals("ServiceIdL3ServiceAccess", connectivity.getServiceAccessCfsId1());
		assertEquals("ServiceIdL3ServiceAccess2", connectivity.getServiceAccessCfsId2());
		assertEquals("l3ServiceAccessPoId-1", connectivity.getL3ServiceAccessId());

		assertEquals("l3ServiceAccessPoId-1", connectivity.getL3ServiceConnectivityAccess().getPoId());

		// 2. Create the COLS Orders
		// -----------------------------------------------------------------------------------------------------------
		// create an update Order to change cpeLanPportName from "cpeLanPortName-1" to "cpeLanPortName-2"
		Order portMovePhysicalOrder = createPortMovePhysicalOrder();

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(portMovePhysicalOrder);

		// Get the order from the DB
		OrderX order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", this.getModifiedSetter()));
		Assert.assertNotNull(order);
		OrderX orderP2PNet = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.002", this.getModifiedSetter()));
		OrderX orderConnectivity = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.003", this.getModifiedSetter()));
		OrderX orderAccess = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.004", this.getModifiedSetter()));

		// Check order state
		assertEquals(OrderState.IN_PROGRESS, order.getState());
		assertEquals(OrderState.FINISHED, orderP2PNet.getState());
		assertEquals(OrderState.FINISHED, orderConnectivity.getState());
		assertEquals(OrderState.IN_PROGRESS, orderAccess.getState());

		OrderDetails od = order.process();
		Assert.assertFalse(od.isHasErrors());

		// 3. OE Notified back the order status
		// -----------------------------------------------------------------------------------------------------------
		this.assertLastNotificationXml("ColsServiceIT_update_portmovePhysicalCpe_notification_inProgress.xml");

		ColsRequestConnectivityPortMovePhysicalCpe aRequest = (ColsRequestConnectivityPortMovePhysicalCpe) order.getDataObjectNotNull(
				ColsProcessConnectivityPortMovePhysicalCpe.class, true)
				.getRequest();

		// Check the CpeLanPortName value on Request.
		assertEquals(newLanPortName, aRequest.getCpeLanPortName());

		// 4. request is already ok for cpeLanPortName, no manual task

		// 5. CoPE Order Process
		// The user's change of step 4 triggered the first order to process.
		// -----------------------------------------------------------------------------------------------------------
		// Check sent order
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_update_l3Access_portmovePhysicalCpe_dual_copeOrder.xml");

		// Make sure main order is not finished yet (first CoPE or should be IN_PROGRESS)
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, orderP2PNet.getState());
		Assert.assertEquals(OrderState.FINISHED, orderConnectivity.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, orderAccess.getState());

		Assert.assertFalse(od.isHasErrors());

		// 5.1 CoPE: Simulate first notification
		// The notification finishes the first CoPE order and this triggers the second

		lcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_update_l3Access_portmovePhysicalCpe_dual2_copeOrder.xml");

		// Make sure the main order is not finished yet (first CoPE or should be FINISHED)
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, orderP2PNet.getState());
		Assert.assertEquals(OrderState.FINISHED, orderConnectivity.getState());
		Assert.assertEquals(OrderState.FINISHED, orderAccess.getState());

		Assert.assertFalse(od.isHasErrors());

		orderP2PNet = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.005", this.getModifiedSetter()));
		orderConnectivity = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.006", this.getModifiedSetter()));
		orderAccess = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001.007", this.getModifiedSetter()));

		// Make sure the main order is not finished yet (second CoPE or should be IN_PROGRESS)
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.FINISHED, orderP2PNet.getState());
		Assert.assertEquals(OrderState.FINISHED, orderConnectivity.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, orderAccess.getState());

		// 5.1 CoPE: Simulate second notification
		// The notification finishes the second CoPE order and this triggers the third

		lcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		// 6. OE-COLS will send a Notification to CFU.
		// -----------------------------------------------------------------------------------------------------------
		this.assertLastNotificationXml("ColsServiceIT_update_portmovePhysicalCpe_notification.xml");

		// Make sure the main order and third CoPE orders are FINISHED)
		Assert.assertEquals(OrderState.FINISHED, orderAccess.getState());
		Assert.assertEquals(OrderState.FINISHED, order.getState());

		// -----------------------------------------------------------------------------------------------------------
		// for connectivity: check if the updates were actually updated on the installed base
		// as well after the notification was received
		connectivity = ServiceLocatorCdi.cdi(L3ServiceConnectivityService.class).searchAll().get(0);
		Assert.assertEquals("l3ServiceConnectivityPoId-1", connectivity.getPoId());

		assertNotNull(connectivity.getL3ServiceConnectivityAccess());
		// both the first and second (DUAL) ServiceAccess has the same oldCpeLanPortName - so both should be updated to the new name:
		Assert.assertEquals(newLanPortName, connectivity.getCpeLanPortName());
		Assert.assertEquals(newLanPortName, connectivity.getCpeLanPortName2());
		// -----------------------------------------------------------------------------------------------------------

		ColsUpdateHistoryRepository updateHistoryRepo = ServiceLocatorCdi.cdi(ColsUpdateHistoryRepository.class, QualifierUtils.COLS_ANNO);
		List<ColsUpdateHistory> updates = updateHistoryRepo.findByColsEntityId(connectivity.getPoId());
		assertEquals(1, updates.size());
		SortedSet<AttributeUpdate> attributes = updates.get(0).getAttributes();
		assertEquals(2, attributes.size());
		assertEquals("l3ServiceConnectivityPoId-1", updates.get(0).getColsEntityId());
		assertEquals("OH-CIH", updates.get(0).getChangeUser());
		assertEquals("CpeLanPortName", attributes.first().getAttributeName());
		assertEquals("cpeLanPortName-1", attributes.first().getOldValue());
		assertEquals("cpeLanPortName-2", attributes.first().getNewValue());
		assertEquals("CpeLanPortName2", attributes.last().getAttributeName());
		assertEquals("cpeLanPortName-1", attributes.last().getOldValue());
		assertEquals("cpeLanPortName-2", attributes.last().getNewValue());
	}

	@Test
	public void checkCpeDataOverwrittenForColsAccessHighEndCreation() {
		OrderX preOrder = this.submitN20(false);

		/********************** ASSIGN CPE ****************************/

		ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).mockSetCpeData(preOrder.getOrderId(), "CpeModel-2", "cpeWanPortName",
				"cpeWanPortNumber", "cpeLanPortPluggable", "cpeLanPortName", "comment", getMetaData());
		ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).setCpeDataDone(preOrder.getOrderId(), getMetaData());

		/*************************************************************/

		String respCpeLanPortPluggable = "respCpeLanPortPluggable";
		String cpeLanPortName = "cpeLanPortName";
		String respCpeWanPortName = "respCpeWanPortName";
		String cpeName = "cpeName";
		String serviceId = "serviceId";
		String omColsSuborderId = "omColsSuborderId";
		String cpeModel = " "; // this is blank so we expect won't override our original value
		String cpeWanPortPluggable = "cpeWanPortPluggable";
		Boolean cpeFinalizedStatus = true;

		ColsProcessAccessCreateHighEnd aProcess = preOrder.getDataObjectNotNull(ColsProcessAccessCreateHighEnd.class, true);

		// ok we do a short cut here to set the responses values received from Tibco
		ServiceLocatorCdi.cdi(NorthboundWebService.class).processS35PreOrderResponse(omColsSuborderId, serviceId, cpeName, cpeModel,
				cpeWanPortPluggable, respCpeWanPortName, respCpeLanPortPluggable, cpeLanPortName, cpeFinalizedStatus, aProcess);

		Assert.assertEquals("CpeModel-2", aProcess.getResponse().getCpe()
				.getCpeModel());

		Assert.assertEquals("cpeName", aProcess.getResponse().getCpe().getCpeName());
	}

	/**
	 * Check that in case of Cols Access LE Creation, the cpe attribute which is set by the manager
	 * and not comming from Tibco Response should stay as it is
	 */
	@Test
	public void checkCpeDataOverwrittenForColsAccessLowEndCreation() {
		OrderX order = this.submitAccessLowEndCopper(false);

		// Simulating the setting of the Cpe Model data
		/********************** ASSIGN CPE ****************************/

		ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).mockSetCpeData(order.getOrderId(), "CpeModel-2", "cpeWanPortName",
				"cpeWanPortNumber", "cpeLanPortPluggable", "cpeLanPortName", "comment", getMetaData());
		ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).setCpeDataDone(order.getOrderId(), getMetaData());

		/*************************************************************/

		String respCpeLanPortPluggable = "respCpeLanPortPluggable";
		String respCpeWanPortName = "respCpeWanPortName";
		String cpeName = "cpeName";
		String serviceId = "serviceId";
		String omColsSuborderId = "omColsSuborderId";
		String cpeModel = " "; // this is blank so we expect won't override our original value
		String cpeWanPortPluggable = "cpeWanPortPluggable";
		String cpeLanPortName = "cpeLanPortName";
		Boolean cpeFinalizedStatus = true;

		ColsProcessAccessCreateLowEnd aProcess = order.getDataObjectNotNull(ColsProcessAccessCreateLowEnd.class, true);

		// ok we do a short cut here to set the responses values received from Tibco
		ServiceLocatorCdi.cdi(NorthboundWebService.class).processS35PreOrderResponse(omColsSuborderId, serviceId, cpeName, cpeModel,
				cpeWanPortPluggable, respCpeWanPortName, respCpeLanPortPluggable, cpeLanPortName, cpeFinalizedStatus, aProcess);

		Assert.assertEquals("CpeModel-2", aProcess.getResponse().getCpe()
				.getCpeModel());

		Assert.assertEquals("cpeName", aProcess.getResponse().getCpe().getCpeName());
	}

	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/CpeLanPortName_ForTranslationCheck.xml" })
	public void testCreateColsLowEndAccessAndExtendL2() throws Exception {
		// ***** CREATE COLSACCESS LOW END ******* //
		OrderX preOrder = create_accessLowEndCopper_until_before_inhouseInstallationOrder(false);

		ColsAccessRepository colsAccessRepository = ServiceLocatorCdi.cdi(ColsAccessRepository.class, QualifierUtils.COLS_ANNO);

		OrderX accessLowEndPreOrderTibcoOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ACCESS_LOW_END_PRE_ORDER.getType());

		// Simulate the GUI Assign of CPE Material. GUI calls same service as below:
		OrderX assignCpeMaterialOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE_MATERIAL.getType());
		/** SET CPE MATERIALS **/
		setCpeMaterial(assignCpeMaterialOrder);

		setCreateColsAccessOrderTasks(preOrder, getMetaData());

		// Check create ColsNorthboundOrder.
		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrder = nbOrderService.findByOrderId("EOE-0000001");
		Assert.assertNotNull(nbOrder);
		ColsProcessAccessCreateLowEnd process = (ColsProcessAccessCreateLowEnd) nbOrder.getColsProcess();

		// Simulate WFM Response
		int wfmId = Integer.valueOf(process.getResponse().getWfmId());
		ServiceLocatorCdi.cdi(ColsWfmSimulator.class).simulateResponse(wfmId, ColsWfmSimulator.CLOSED);

		// Activate Alarm
		ServiceLocatorCdi.cdi(ColsOrderAlarmActivationService.class).activateAlarm(preOrder.getOrderId());

		Assert.assertEquals("main order be FINISHED", OrderState.FINISHED, preOrder.getState());
		Assert.assertEquals("confirm order should be finished", OrderState.FINISHED, accessLowEndPreOrderTibcoOrder.getState());
		Assert.assertEquals("main order should be ", OrderState.FINISHED, accessLowEndPreOrderTibcoOrder.getParentX().getState());
		Assert.assertEquals("main order should be ", OrderState.FINISHED, preOrder.getState());

		// last notofication, now only one LanPortPluggable and one LanPortName should show
		this.assertLastNotificationXml("ColsServiceIT_access_low_end_preorder_ready_notification.xml");

		// There should be one access created due to the confirmation
		assertEquals(1, colsAccessRepository.count());

		ColsAccessService colsAccessService = ServiceLocatorCdi.cdi(ColsAccessService.class);
		List<ColsAccess> accesses = colsAccessService.searchAll();
		assertEquals(1, accesses.size());

		assertLowEndAccessAttributeValues(accesses.get(0), false);

		ColsResponseAccessCreateLowEnd aResponses = (ColsResponseAccessCreateLowEnd) nbOrder.getColsProcess().getResponse();
		Assert.assertEquals("COL:ACC:1", aResponses.getColsAccessId());

		ColsAccess colsAccess = ServiceLocatorCdi.cdi(ColsAccessService.class).searchByColsAccessId("COL:ACC:1");
		this.testData().cols()
				.newColsCpeModel(colsAccess.getLogicalPhysicalCpe().getCpe().getCpeModel(), colsAccess.getCpeOption());

		// ***** CREATE L2 NET ******* //
		new OoeOrderColsFacadeIT().mockExistingVLanServiceId();

		Order newColsL2NetOrder = OrderCreation.createL2NetOrder();
		new ColsService().submitOrder(newColsL2NetOrder);

		OrderX l2NetOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002", this.getModifiedSetter()));

		OrderDetails od = l2NetOrder.process();
		Assert.assertFalse(od.isHasErrors());

		Assert.assertEquals(OrderState.IN_PROGRESS, l2NetOrder.getState());

		OrderType submittedOrder = this.pollLastRequest(OrderType.class);
		new LcsNotificationHandler().simulateNotification(submittedOrder);

		Assert.assertEquals(OrderState.FINISHED, l2NetOrder.getState());

		nbOrder = nbOrderService.findByOrderId("EOE-0000002");
		Assert.assertNotNull(nbOrder);

		ColsResponseNetCreateL2 aNetResponses = (ColsResponseNetCreateL2) nbOrder.getColsProcess().getResponse();
		Assert.assertEquals("COL:SRV:1", aNetResponses.getColsServiceId());

		// ***** CREATE L2 EXTEND ******* //
		String extendNetOrderXml = new IOUtil().loadTextFromUrl(JuUrl.resourceRelativeTo(
				"ColsService_extendColsNet_previousColAccessLec.xml",
				this.getClass()));
		Order extendNetOrder = XmlUtils.marshaller().unmarshal(extendNetOrderXml, Order.class);

		new ColsService().submitOrder(extendNetOrder);

		OrderX extendL2Order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003", this.getModifiedSetter()));

		setChangeCpeOption("EOE-0000003.001", ColsConstants.ChangeCpeOption.NEXT_STEP);

		mockCpeLanPortNameDone("EOE-0000003");

		Assert.assertEquals(OrderState.IN_PROGRESS, extendL2Order.getState());

		submittedOrder = this.pollLastRequest(OrderType.class);
		new LcsNotificationHandler().simulateNotification(submittedOrder);

		Assert.assertEquals(OrderState.FINISHED, extendL2Order.getState());
	}

	/** Change CAN Portmove from UI **/
	/** Testing EoeOrderServiceCols.submitChangeBusinessPortmoveOrder() for CAN **/

	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/cpeData.xml" })
	public void testSubmitChangeCanPortmoveOrder() throws Exception {

		this.testCreateColsLowEndAccessAndExtendL2();

		ColsAccess colsAccess = ServiceLocatorCdi.cdi(ColsAccessRepository.class, QualifierUtils.COLS_ANNO).findAll().get(0);
		assertNotNull(colsAccess);

		L2ServiceConnectivityService connectivityService = ServiceLocatorCdi.cdi(L2ServiceConnectivityService.class);
		List<L2ServiceConnectivity> connectivities = connectivityService.searchByColsAccessId(colsAccess.getColsAccessId());
		assertEquals(1, connectivities.size());

		L2ServiceConnectivity l2ServiceConnectivity = connectivities.get(0);
		assertNotNull(l2ServiceConnectivity);
		l2ServiceConnectivity.getL2ServiceAccess().setL2ServiceConnectivity(l2ServiceConnectivity);
		connectivityService.update(l2ServiceConnectivity);

		ColsRequestNG colsRequest = new EoeOrderServiceColsBean().getColsAccessRequestBy(colsAccess);
		new EoeOrderServiceColsBean().submitChangeBusinessPortmoveOrder(colsRequest, colsAccess, "CanName", "CanPort", "PortDefect",
				ColsConstants.CAN.toString());

		OrderX order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000004", this.getModifiedSetter()));
		Assert.assertNotNull(order);
		OrderX businessPortmove = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000004.001", this.getModifiedSetter()));
		Assert.assertNotNull(businessPortmove);

		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());

		// simulate port move finished
		new PortMoveTestUtil().notifyPortMoveEC_or_CANComplete("EOE-0000004", "CanName", "CanPort");

		Assert.assertEquals(OrderState.FINISHED, order.getState());
		Assert.assertEquals(OrderState.FINISHED, businessPortmove.getState());

		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrder = nbOrderService.findByOrderId("EOE-0000004");
		Assert.assertNotNull(nbOrder);
		Assert.assertEquals(1, nbOrder.getSuborders().size());

		ColsRequestConnectivityPortMoveCanMcan aRequest = (ColsRequestConnectivityPortMoveCanMcan) nbOrder.getColsProcess().getRequest();

		assertEquals("COLS123", aRequest.getOrderHeaderData().getColsId());
		assertEquals("6666", aRequest.getOrderHeaderData().getFullfillmentEngineCorrelationId());
		assertEquals("OE-COLS", aRequest.getOrderHeaderData().getOrderCreatorApplication());
		assertEquals("1234can", aRequest.getOrderHeaderData().getOrderId());
		assertEquals("1234", aRequest.getOrderHeaderData().getOrderItemId());
		assertEquals("COL:ACC:1", aRequest.getColsAccessId());
		assertEquals("CanName", aRequest.getNodeInformation().getNodeName());
		assertEquals("CanPort", aRequest.getNodeInformation().getNodePort());
		assertEquals("PortDefect", aRequest.getReason());
		assertEquals("CAN", aRequest.getNodeInformation().getNodeType());
		assertNotNull(aRequest.getRfsDate());
		assertEquals("BusinessPortMove", aRequest.getSubAction());

		ColsResponseConnectivityPortMoveCanMcan aResponse = (ColsResponseConnectivityPortMoveCanMcan) nbOrder.getColsProcess()
				.getResponse();
		Assert.assertEquals("CanName", aResponse.getCanName());
		Assert.assertEquals("CanPort", aResponse.getCanPort());

		// Check ColsAccess
		Assert.assertEquals("CanName", colsAccess.getMscLeg().getNePort().getNeName());
		Assert.assertEquals("CanPort", colsAccess.getMscLeg().getNePort().getNePort());
	}

	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/cpeData.xml" })
	public void test_CreateColsAccessHE_Sip_Mediant() throws ObjectNotFoundException {
		test_CreateColsAccessHE_Sip_Mediant(null, null);
	}

	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/cpeData.xml" })
	public void test_CreateColsAccessHE_Sip_Mediant_With_Modified_NumberOfPriBri() throws ObjectNotFoundException {
		test_CreateColsAccessHE_Sip_Mediant(4, null);
	}

	@Test(expected = Exception.class)
	@DataSet(value = "masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/cpeData.xml" })
	public void test_CreateColsAccessHE_Sip_Mediant_With_Invalid_ChangeNumberBriPriBothSet() throws ObjectNotFoundException {
		try {
			test_CreateColsAccessHE_Sip_Mediant(1, 2);
			Assert.fail("test should fail");
		} catch (Exception e) {
			Assert.assertTrue(ErrorInfoUtil.getRootCause(e) instanceof IllegalArgumentException);
		}
	}

	private void test_CreateColsAccessHE_Sip_Mediant(Integer newNumberOfBri, Integer newNumberOfPri) throws ObjectNotFoundException {

		// Load Sample northbound order XML
		String newColsAccessOrderXml = new IOUtil().loadTextFromUrl(JuUrl.resourceRelativeTo(
				"ColsService_CreateColsAccessHE_SipMediant.xml", ColsServiceCommonBehaviorTest.class));

		Order newColsOrder = XmlUtils.marshaller().unmarshal(newColsAccessOrderXml, Order.class);

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(newColsOrder);

		// Get the order that was implicitly created by the ColsService
		OrderX preOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", this.getModifiedSetter()));

		// Get SubOrders we will make checks on
		OrderX s33PreOrderTibcoOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_S33_PRE_ORDER.getType());
		OrderX n20PreOrderConfirmedOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_N20_PRE_ORDER_CONFIRMED.getType());

		// Process the order to make sure it's processed without errors
		OrderDetails od = preOrder.process();
		Assert.assertFalse(od.isHasErrors());

		// Check create ColsNorthboundOrder.
		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		try {
			ColsNorthboundOrder nbOrder = nbOrderService.findByOrderId("EOE-0000001");
			Assert.assertNotNull(nbOrder);
			Assert.assertEquals(5, nbOrder.getSuborders().size());
		} catch (ObjectNotFoundException e) {
			e.printStackTrace();
		}

		// Make sure the S33 suborder is waiting (for the CPE Model assignment)
		Assert.assertEquals(OrderState.WAITING, s33PreOrderTibcoOrder.getState());

		// The SubOrder COLS_ORDER_ASSIGN_CPE should be in state READY
		OrderX assignCpeOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE.getType());
		Assert.assertEquals(OrderState.IN_PROGRESS, assignCpeOrder.getState());

		// **** If you want to test the UI until this point:
		// CpeModelTestdataCreator.setupMatchCpeLanPortAndModel("FastEthernet0/1", "CpeModel-1", "HighEnd", false, true, 6000, null);
		// CpeModelTestdataCreator.createCpeMaterial("HighEnd", "CpeModel-1");

		// Simulate the GUI Assign of CPE Data. GUI calls same service as below:
		setCpeDataForMediantWithChangeNumberOfPriBri(preOrder, newNumberOfBri, newNumberOfPri);

		Assert.assertEquals(OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getState());

		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_n20_copeOrder_1plug_sipMediant.xml");

		// Check Main Order ready notification XML
		// below should be changed to a notification for only one CpeLan - currently it has 3, like original NB order
		this.assertLastNotificationXml("ColsServiceIT_n20_main_ready_notification.xml");

		// The main order should be in progress
		Assert.assertEquals(OrderState.IN_PROGRESS, preOrder.getState());

		// S33 Preorder should be in progress, waiting for manual update of the data
		Assert.assertEquals(OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getState());

		// ConfirmPreOrder should be waiting as we don't have the necessary data to confirm/cancel yet
		Assert.assertEquals(OrderState.WAITING, n20PreOrderConfirmedOrder.getState());
		// Phase Id should already be in progress
		OrderHeaderDataNG ohd = preOrder.getDataObjectNotNull(ColsProcessAccessCreateHighEnd.class, true).getRequest()
				.getOrderHeaderData();
		Assert.assertEquals(Constants.COLS_ORDER_PHASE_PREORDER_IN_PROGRESS, ohd.getPhaseId());

		// Simulate notification from SouthBound
		OrderType submittedOrder = this.pollLastRequest(OrderType.class);

		// First, CoPE will send an intermediate notification of type SubmitOrderAck with status succeeded,
		// containing one OrderLineItem result for the PO_MSC_HIGH_END_LEG with status succeeded, containing the attribute
		// circuitId.
		// As soon as the circuitId is known, we need to set it on the responses object and send a notification to CFU.

		new LcsNotificationHandler().simulateNotification(submittedOrder, NotificationType.SUBMIT_ORDER_ACK);

		// Make sure intermediate notification was sent
		// this.assertion().cope().lastNotificationStati("1234", "in progress", "in progress", "preorder");

		// Make sure the circuitId was stored to the responses object
		ColsResponseAccessCreateHighEnd response = preOrder.getDataObjectNotNull(
				ColsProcessAccessCreateHighEnd.class, true)
				.getResponse();
		Assert.assertNotNull(response.getCircuitId());

		new LcsNotificationHandler().simulateNotification(submittedOrder);

		// Check Pre Order ready notification XML
		this.assertLastNotificationXml("ColsServiceIT_n20_preorder_ready_notification.xml");

		// S33 PreOrder should be in progress now, phase still in progress
		Assert.assertEquals(OrderState.IN_PROGRESS, n20PreOrderConfirmedOrder.getState());
		Assert.assertEquals(Constants.COLS_ORDER_PHASE_PREORDER_IN_PROGRESS, ohd.getPhaseId());

		ColsAccessRepository colsAccessRepository = ServiceLocatorCdi.cdi(ColsAccessRepository.class, QualifierUtils.COLS_ANNO);
		// There should be no access created since this is the preordering
		assertEquals(0, colsAccessRepository.count());

		// Check cpe response values
		response = preOrder.getDataObjectNotNull(ColsProcessAccessCreateHighEnd.class, true)
				.getResponse();
		Assert.assertNotNull(response.getCpe().getCpeModel());
		Assert.assertNotNull(response.getCpe().getCpeName());
		Assert.assertNotNull(response.getCpe().getCpeWanPortPluggable());
		Assert.assertNotNull(response.getCpe().getCpeWanPortName());
		Assert.assertNotNull(response.getCpe().getCpeLanPortPluggable());
		Assert.assertNotNull(response.getCpe().getCpeLanPortName());
		Assert.assertNotNull(response.getCpe().getMediantModel());
		Assert.assertNotNull(response.getCpe().getMediantSlot());

		// Confirm this order through the EoeOrderServiceCols

		ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).confirmPreOrder(s33PreOrderTibcoOrder.getOrderId(), getMetaData());

		OrderX n20ConfirmOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002", getModifiedSetter()));
		Assert.assertNotNull(n20ConfirmOrder);

		Assert.assertEquals(OrderState.IN_PROGRESS, n20ConfirmOrder.getSubOrderX(ColsOrderType.COLS_CPE_ORDER_DONE.getType()).getState());
		setCpeOrderDone(n20ConfirmOrder);

		Assert.assertEquals(OrderState.FINISHED, n20ConfirmOrder.getSubOrderX(ColsOrderType.COLS_CPE_ORDER_DONE.getType()).getState());

		// Simulate the GUI Assign of CPE Material. GUI calls same service as below:
		OrderX assignCpeMaterialOrder = n20ConfirmOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE_MATERIAL.getType());
		/** SET CPE MATERIALS **/
		setCpeMaterial(assignCpeMaterialOrder);

		setCreateColsAccessOrderTasks(n20ConfirmOrder, getMetaData());

		ColsNorthboundOrder nbOrder = nbOrderService.findByOrderId("EOE-0000002");
		ColsProcessAccessConfirm process = (ColsProcessAccessConfirm) nbOrder.getColsProcess();

		// Simulate WFM Response
		int wfmId = Integer.valueOf(process.getResponse().getWfmId());
		ServiceLocatorCdi.cdi(ColsWfmSimulator.class).simulateResponse(wfmId, ColsWfmSimulator.CLOSED);

		// Get the ColsResponse from the WFM notification
		ColsResponse wfmColsResponse = this.getRequestHolder().pollRequest(ColsResponse.class);
		Boolean wfmEnabled = AttributeManagerUtil.getSystemPropertyBoolean("cols.wfm.ennabled");
		if (wfmEnabled) {
			Assert.assertTrue(wfmColsResponse.isSuccessful());
		}

		// Make sure the preOrderConfirmed order is still in progress and the phase is confirmed
		Assert.assertEquals(OrderState.IN_PROGRESS, n20PreOrderConfirmedOrder.getState());
		Assert.assertEquals(Constants.COLS_ORDER_PHASE_PREORDER_CONFIRMED, ohd.getPhaseId());

		// check sent order : to fix the xml
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_n20_copePreOrderConfirm.xml");

		// Main Order should stay in IN_PROGRESS
		Assert.assertEquals("main order should still be in IN_PROGRESS", OrderState.IN_PROGRESS, s33PreOrderTibcoOrder.getParentX()
				.getState());

		Assert.assertEquals("confirm order should be in process ", OrderState.IN_PROGRESS, n20ConfirmOrder.getState());

		// Simulate CoPE Notification
		new LcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		// Activate Alarm
		ServiceLocatorCdi.cdi(ColsOrderAlarmActivationService.class).activateAlarm(n20ConfirmOrder.getOrderId());

		Assert.assertEquals("checking n20ConfirmOrder", OrderState.FINISHED, n20ConfirmOrder.getState());
		Assert.assertEquals("confirm order should be finished", OrderState.FINISHED, s33PreOrderTibcoOrder.getState());
		Assert.assertEquals("main order should be ", OrderState.FINISHED, s33PreOrderTibcoOrder.getParentX().getState());
		Assert.assertEquals("main order should be ", OrderState.FINISHED, preOrder.getState());

		// There should be one access created due to the confirmation
		assertEquals(1, colsAccessRepository.count());

		// Check notification XML
		this.assertLastNotificationXml("ColsServiceIT_n20_notification.xml");

		ColsAccessService colsAccessService = ServiceLocatorCdi.cdi(ColsAccessService.class);
		List<ColsAccess> accesses = colsAccessService.searchAll();
		assertEquals(1, accesses.size());

		// Check values
		ColsAccess colsAccess = accesses.get(0);
		assertEquals("unrelated", colsAccess.getAccessType());
		assertEquals("123", colsAccess.getCustomerId());
		assertEquals("HighEndFiber", colsAccess.getServiceRealizationType());
		assertEquals("HighEnd", colsAccess.getCpeOption());
		assertEquals("0", colsAccess.getCustomerWindow());
		assertEquals("ServiceIdLogicalPhysicalCpe", colsAccess.getLogicalPhysicalCpe().getCfsIdLogicalPhysicalCpe());
		assertEquals("AC0000001", colsAccess.getAccessId());
		assertEquals("COL:ACC:1", colsAccess.getColsAccessId());
		assertEquals("ALL:SUB:1", colsAccess.getMscLeg().getMscLegId());
		assertEquals("ALL:SUB:2", colsAccess.getLogicalPhysicalCpe().getLogicalPhysicalCpeId());

		// Check Customer values
		assertEquals("UBS AG", colsAccess.getColsSite().getCompanyName());
		assertEquals("Bahnhofstrasse", colsAccess.getColsSite().getStreet());
		assertEquals("100", colsAccess.getColsSite().getHouseNumber());
		assertEquals("Bern", colsAccess.getColsSite().getCity());
		assertEquals("300800", colsAccess.getColsSite().getPostalCode());
		assertEquals("Peter Schuhmacher", colsAccess.getColsSite().getContactPerson());
		assertEquals("0792315847", colsAccess.getColsSite().getContactTelephone());
		assertEquals("Peter.Schuhmacher@bluewin.ch", colsAccess.getColsSite().getEmail());

		// Check Dealer values
		assertEquals("Martin Lala", colsAccess.getDealer().getName());
		assertEquals("033352352", colsAccess.getDealer().getPhone());
		assertEquals("033352353", colsAccess.getDealer().getFax());
		assertEquals("Martin.Lala@bluewin.ch", colsAccess.getDealer().getEmail());

		// Check Cpe Values
		assertEquals("CpeModel-1", colsAccess.getLogicalPhysicalCpe().getCpe().getCpeModel());
		assertEquals("CpeName-1", colsAccess.getLogicalPhysicalCpe().getCpe().getCpeName());
		assertEquals("CpeWanPortName-1", colsAccess.getLogicalPhysicalCpe().getCpe().getCpeWanPortName());
		assertEquals("CpeWanPortPluggable-1", colsAccess.getLogicalPhysicalCpe().getCpe().getCpeWanPortPluggable());
		assertEquals("cpeLanPortName1", colsAccess.getLogicalPhysicalCpe().getCpe().getCpeLanPortName());
		assertEquals("cpeLanPortPluggable1", colsAccess.getLogicalPhysicalCpe().getCpe().getCpeLanPortPluggable());
		assertEquals("MediantModel-1", colsAccess.getLogicalPhysicalCpe().getCpe().getMediantModel().getName());
		assertEquals("MediantSlot-1", colsAccess.getLogicalPhysicalCpe().getCpe().getMediantSlot().getName());

		// Check CableBoxData
		final MscHeLeg mscHeLeg = (MscHeLeg) colsAccess.getMscLeg();
		assertNotNull(mscHeLeg.getCableBoxData());
		assertEquals("CableBox-1", mscHeLeg.getCableBoxData().getCableBox());
		assertEquals("ContactPoint1-1", mscHeLeg.getCableBoxData().getContactPoint1());
		assertEquals("ContactPoint2-1", mscHeLeg.getCableBoxData().getContactPoint2());

		// Check Sip Mediant attributes
		assertEquals("yes", colsAccess.getOptionMediant());
		if (ObjectUtil.coalesce(newNumberOfBri, newNumberOfPri) == null) {
			assertEquals(Integer.valueOf(50), colsAccess.getNumberOfBRI());
		} else {
			// check number pri / bri
			assertEquals(newNumberOfBri, colsAccess.getNumberOfBRI());
			assertEquals(newNumberOfPri, colsAccess.getNumberOfPRI());
		}
		assertNull(colsAccess.getNumberOfPRI());

		// Check ServiceEnabling
		assertEquals("FirstNet", colsAccess.getServiceEnablingType());
		assertEquals("2", colsAccess.getServiceEnablingClass());
	}

	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/cpeData.xml" })
	public void test_CreateColsAccessLEC_Sip_Mediant() throws ObjectNotFoundException {
		test_CreateColsAccessLEC_Sip_Mediant_With_ChangetNumberOfBriPri(null, null);
	}

	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/cpeData.xml" })
	public void test_CreateColsAccessLEC_Sip_Mediant_With_Change_NumberOfBriPri() throws ObjectNotFoundException {
		test_CreateColsAccessLEC_Sip_Mediant_With_ChangetNumberOfBriPri(null, 4);
	}

	private void test_CreateColsAccessLEC_Sip_Mediant_With_ChangetNumberOfBriPri(Integer newNumberOfBri, Integer newNumberOfPri)
			throws ObjectNotFoundException {
		String newColsAccessLowEndCopperOrderXml = new IOUtil().loadTextFromUrl(JuUrl.resourceRelativeTo(
				"ColsService_CreateColsAccessLEC_SipMediant.xml",
				ColsServiceCommonBehaviorTest.class));

		Order newColsOrder = XmlUtils.marshaller().unmarshal(newColsAccessLowEndCopperOrderXml, Order.class);

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(newColsOrder);

		// Get the order that was implicitly created by the ColsService
		OrderX preOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", this.getModifiedSetter()));

		EoeOrderServiceCols eoeOrderServiceCols = ServiceLocatorCdi.cdi(EoeOrderServiceCols.class);

		// Get SubOrders we will make checks on
		OrderX accessLowEndPreOrderTibcoOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ACCESS_LOW_END_PRE_ORDER.getType());

		// Process the order to make sure it's processed without errors
		OrderDetails od = preOrder.process();
		Assert.assertFalse(od.isHasErrors());

		// Check create ColsNorthboundOrder.
		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrder = null;
		try {
			nbOrder = nbOrderService.findByOrderId("EOE-0000001");
			Assert.assertNotNull(nbOrder);
			Assert.assertEquals(15, nbOrder.getSuborders().size());
		} catch (ObjectNotFoundException e) {
			e.printStackTrace();
		}

		// Assign user
		ColsUser user = createTestUser(-200L, "testuser1", "test1", "user1");
		nbOrder.setAssignedUser(user);
		ColsOrderRepository colsOrderRepository = ServiceLocatorCdi.cdi(ColsOrderRepository.class, QualifierUtils.COLS_ANNO);
		colsOrderRepository.save(nbOrder);

		// Suborder ColsQualyByAddress should be in status IN_PROGRESS, waiting for the COLS GUI to assign the UP Data
		OrderX colsQualyByAddressOrder = preOrder.getSubOrderX(ColsOrderType.COLS_QUALI_BY_ADDRESS.getType());
		Assert.assertEquals(OrderState.IN_PROGRESS, colsQualyByAddressOrder.getState());

		// Call EoeOrderServiceCols service to set UP data

		UpData upData = new UpData();
		upData.setAccessNet("accessNet");
		upData.setMaxAccessSpeedDown("100M"); // Format?
		upData.setMaxAccessSpeedUp("100M"); // Format?
		upData.setSse("1");
		upData.setTaxRegion("2");
		upData.setUnitNumber("3");
		upData.setUnitType("4");
		upData.setUpPreparation("upPrep");

		// Make sure we can get an UpData list
		ColsSite site = eoeOrderServiceCols.getSite(colsQualyByAddressOrder.getOrderId(), this.getMetaData());
		List<UpData> upDatas = eoeOrderServiceCols.getUpDataList(
				colsQualyByAddressOrder.getOrderId(), this.getMetaData(),
				site.getStreet(), site.getHouseNumber(),
				OrderDetailPm.convertPostalCode(site.getPostalCode()), site.getCity());

		Assert.assertTrue(upDatas.size() > 0);
		eoeOrderServiceCols.saveUpData(colsQualyByAddressOrder.getOrderId(), upData, this.getMetaData());

		// ColsQualyByAddress should be finished now
		Assert.assertEquals(OrderState.FINISHED, colsQualyByAddressOrder.getState());

		// Suborder ColsQualiByStartPoint should be in status IN_PROGRESS, waiting for the COLS GUI to assign time slog
		OrderX colsQualiByStartPointOrder = preOrder.getSubOrderX(ColsOrderType.COLS_QUALI_BY_START_POINT.getType());
		Assert.assertEquals(OrderState.IN_PROGRESS, colsQualiByStartPointOrder.getState());

		// Call EoeOrderServiceCols service to assign time slot
		ColsTimeSlot timeSlot = new ColsTimeSlot();
		timeSlot.setFulfillmentTimeSlotQualifyIndex(1L);
		timeSlot.setQualificationIndex(2L);
		timeSlot.setQualificationNumber(3L);

		// Make sure we can get a time slot list
		List<ColsTimeSlot> timeSlots = eoeOrderServiceCols.getTimeSlotList(colsQualiByStartPointOrder.getOrderId(), this.getMetaData());
		Assert.assertTrue(timeSlots.size() > 0);
		eoeOrderServiceCols.saveTimeSlot(colsQualiByStartPointOrder.getOrderId(), timeSlot, this.getMetaData(), "testuser1");

		// ColsQualyByAddress should be finished now
		Assert.assertEquals(OrderState.FINISHED, colsQualiByStartPointOrder.getState());

		// Simulate the GUI Assign of CPE Data. GUI calls same service as below:
		OrderX assignCpeOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE.getType());
		Assert.assertNotNull(assignCpeOrder);

		/** SET CPE INFORMATION **/
		setCpeDataForMediantWithChangeNumberOfPriBri(assignCpeOrder, newNumberOfBri, newNumberOfPri);

		// Check the COPE order XML - one plug should show here
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_access_low_end_copeOrder_sipMediant.xml");

		// Check Main Order ready notification XML - first notification: original 3 plugs should show here
		this.assertLastNotificationXml("ColsServiceIT_access_low_end_main_ready_notification.xml"); // really expected

		// The main order should be in progress
		Assert.assertEquals(OrderState.IN_PROGRESS, preOrder.getState());

		// Access Low End Preorder should be in progress, waiting for manual update of the data

		Assert.assertEquals(OrderState.IN_PROGRESS, accessLowEndPreOrderTibcoOrder.getState());

		// Phase Id should already be in progress
		OrderHeaderDataNG ohd = preOrder.getDataObjectNotNull(ColsProcessAccessCreateLowEnd.class, true).getRequest()
				.getOrderHeaderData();
		Assert.assertEquals(Constants.COLS_ORDER_PHASE_PREORDER_IN_PROGRESS, ohd.getPhaseId());

		// Simulate notification from SouthBound
		OrderType submittedOrder = this.pollLastRequest(OrderType.class);
		new LcsNotificationHandler().simulateNotification(submittedOrder);

		// WFM response will be simulated automatically

		// Check Pre Order ready notification XML
		this.assertLastNotificationXml("ColsServiceIT_access_ready_notification_beforeInhouse.xml");

		// Access Low End PreOrder should be confirmed since there is no confirmation order
		Assert.assertEquals(Constants.COLS_ORDER_PHASE_PREORDER_CONFIRMED, ohd.getPhaseId());

		ColsAccessRepository colsAccessRepository = ServiceLocatorCdi.cdi(ColsAccessRepository.class, QualifierUtils.COLS_ANNO);
		// There should be no access created since this is the preordering
		assertEquals(1, colsAccessRepository.count());

		// Make sur the preOrderConfirmed order is still in progress and the phase is confirmed
		Assert.assertEquals(Constants.COLS_ORDER_PHASE_PREORDER_CONFIRMED, ohd.getPhaseId());

		Assert.assertEquals("main order be FINISHED", OrderState.FINISHED, accessLowEndPreOrderTibcoOrder.getState());

		// Check cpe response values
		ColsResponseAccessCreateLowEnd response = preOrder.getDataObjectNotNull(
				ColsProcessAccessCreateLowEnd.class, true)
				.getResponse();
		Assert.assertNotNull(response.getCpe().getCpeModel());
		Assert.assertNotNull(response.getCpe().getCpeName());
		Assert.assertNotNull(response.getCpe().getCpeWanPortPluggable());
		Assert.assertNotNull(response.getCpe().getCpeWanPortName());
		Assert.assertNotNull(response.getCpe().getCpeLanPortPluggable());
		Assert.assertNotNull(response.getCpe().getCpeLanPortName());
		Assert.assertNotNull(response.getCpe().getMediantModel());
		Assert.assertNotNull(response.getCpe().getMediantSlot());

		// Main Order should be in progress
		Assert.assertEquals("main order be IN PROGRESS", OrderState.IN_PROGRESS, preOrder.getState());

		accessLowEndPreOrderTibcoOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ACCESS_LOW_END_PRE_ORDER.getType());

		// Simulate the GUI Assign of CPE Material. GUI calls same service as below:
		OrderX assignCpeMaterialOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE_MATERIAL.getType());
		/** SET CPE MATERIALS **/
		setCpeMaterial(assignCpeMaterialOrder);
		setCreateColsAccessOrderTasks(preOrder, getMetaData());

		ColsProcessAccessCreateLowEnd process = (ColsProcessAccessCreateLowEnd) nbOrder.getColsProcess();

		// Simulate WFM Response
		int wfmId = Integer.valueOf(process.getResponse().getWfmId());
		ServiceLocatorCdi.cdi(ColsWfmSimulator.class).simulateResponse(wfmId, ColsWfmSimulator.CLOSED);

		// Activate Alarm
		ServiceLocatorCdi.cdi(ColsOrderAlarmActivationService.class).activateAlarm(preOrder.getOrderId());

		Assert.assertEquals("main order be FINISHED", OrderState.FINISHED, preOrder.getState());
		Assert.assertEquals("confirm order should be finished", OrderState.FINISHED, accessLowEndPreOrderTibcoOrder.getState());
		Assert.assertEquals("main order should be ", OrderState.FINISHED, accessLowEndPreOrderTibcoOrder.getParentX().getState());
		Assert.assertEquals("main order should be ", OrderState.FINISHED, preOrder.getState());

		// last notofication, now only one LanPortPluggable and one LanPortName should show
		this.assertLastNotificationXml("ColsServiceIT_access_low_end_preorder_ready_notification.xml");

		// There should be one access created due to the confirmation
		assertEquals(1, colsAccessRepository.count());

		ColsAccessService colsAccessService = ServiceLocatorCdi.cdi(ColsAccessService.class);
		List<ColsAccess> accesses = colsAccessService.searchAll();
		assertEquals(1, accesses.size());

		ColsAccess colsAccess = accesses.get(0);
		assertLowEndAccessAttributeValues(colsAccess, false);
		assertEquals("yes", colsAccess.getOptionMediant());

		// check number of pri / bri
		if (ObjectUtil.coalesce(newNumberOfBri, newNumberOfPri) == null) {
			assertEquals(Integer.valueOf(28), colsAccess.getNumberOfBRI());
			assertNull(colsAccess.getNumberOfPRI());
		} else {
			assertEquals(newNumberOfBri, colsAccess.getNumberOfBRI());
			assertEquals(newNumberOfPri, colsAccess.getNumberOfPRI());
		}
	}

	@Test
	public void test_CreateColsAccessHE_with_InvalidNumberOfBriAndPri() throws Exception {
		ErroneousOrderService aService = ServiceLocatorCdi.cdi(ErroneousOrderService.class);
		assertEquals(0, aService.searchAll().size());

		String newColsAccessOrderXml = new IOUtil().loadTextFromUrl(JuUrl
				.resourceRelativeTo("ColsService_colsAccess_invalidNumberOfBriAndPri.xml", ColsServiceCommonBehaviorTest.class));

		Order colsAccessOrder = XmlUtils.marshaller().unmarshal(newColsAccessOrderXml, Order.class);

		new ColsService().submitOrder(colsAccessOrder);

		// Get the order that was implicitly created by the ColsService
		OrderX preOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", this.getModifiedSetter()));

		Assert.assertEquals("main order be TO_REVISE", OrderState.TO_REVISE, preOrder.getState());
		List<EswLog> eswLogs = serviceLocator.cdi(EoeOrderServiceCols.class).getEswLog((long) 1);
		EswLog failureLog = null;
		for (EswLog aLog : eswLogs) {
			failureLog = aLog;
			if (aLog.getMessage().contains("set to revise")) {
				break;
			}
		}
		assertEquals("The order is invalid because both attributes NumberOfPRI and NumberOfBRI are provided.",
				failureLog.getMessageLong());
	}

	@Test
	public void test_CreateColsAccessHE_with_ValidNumberOfBriAndPri() throws Exception {
		ErroneousOrderService aService = ServiceLocatorCdi.cdi(ErroneousOrderService.class);
		assertEquals(0, aService.searchAll().size());

		String newColsAccessOrderXml = new IOUtil().loadTextFromUrl(JuUrl
				.resourceRelativeTo("ColsService_colsAccess_validNumberOfBriAndPri.xml", ColsServiceCommonBehaviorTest.class));

		Order colsAccessOrder = XmlUtils.marshaller().unmarshal(newColsAccessOrderXml, Order.class);

		new ColsService().submitOrder(colsAccessOrder);

		OrderX preOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", this.getModifiedSetter()));
		assertNotNull(preOrder);
		assertEquals(0, aService.searchAll().size());
	}

	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/cpeData.xml" })
	public void test_ManualTask() throws ObjectNotFoundException {
		String newColsAccessOrderXml = new IOUtil().loadTextFromUrl(JuUrl.resourceRelativeTo(
				"ColsService_CreateColsAccessHE_SipMediant.xml", ColsServiceCommonBehaviorTest.class));
		Order newColsOrder = XmlUtils.marshaller().unmarshal(newColsAccessOrderXml, Order.class);

		new ColsService().submitOrder(newColsOrder);

		OrderX preOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", this.getModifiedSetter()));
		OrderDetails od = preOrder.process();
		Assert.assertFalse(od.isHasErrors());

		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel("FastEthernet0/1", "CpeModel-1", "HighEnd", true, true, 6000, null);
		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel("FastEthernet0/2", "CpeModel-1", "HighEnd", true, true, 6000, null);
		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel("cpeLanPortName1", "CpeModel-1", "HighEnd", true, true, 6000, null);
		CpeModelTestdataCreator.createCpeMaterial("HighEnd", "CpeModel-1");

		CpeModelTestdataCreator.setupMatchCpeLanPortAndModel("FastEthernet0/1", "CpeModel-2", "HighEnd", true, true, 6000, null);
		CpeModelTestdataCreator.createCpeMaterial("HighEnd", "CpeModel-2");
	}

	@Test
	public void trySubmitConfirmColsAccessOrder_With_MissingAttributes() throws Exception {
		testContext().eoeDefault().setCopeNotificationSimulation(NotificationSimulationType.AUTOMATIC);
		
		OrderX preOrder = this.submitN20(false);

		setCpeDataWithOnePlug(preOrder, false);

		OrderDetails od = preOrder.process();
		Assert.assertFalse(od.isHasErrors());

		String confirmOrderXml = new IOUtil().loadTextFromUrl(JuUrl.resourceRelativeTo("ColsService_confirmColsAccess_without_RfsDate.xml",
				this.getClass()));

		Order confirmOrder = XmlUtils.marshaller().unmarshal(confirmOrderXml, Order.class);

		new ColsService().submitOrder(confirmOrder);

		ErroneousOrderService aService = ServiceLocatorCdi.cdi(ErroneousOrderService.class);
		ErroneousOrder anErroneousOrder = aService.searchAll().get(0);
		assertEquals(2, aService.searchAll().size());
		assertNotNull(anErroneousOrder.getXml());
		assertNotNull(anErroneousOrder.getReceptionDate());
		assertEquals(1, anErroneousOrder.getColsErrors().size());
		assertEquals("MISSING_OR_INVALID_ATTRIBUTES (errorCode: 2)", anErroneousOrder.getColsErrors().first().getCode());
		assertEquals("Missing or invalid attributes in order 378 : [ColsId, RfsDate]", anErroneousOrder.getColsErrors().first()
				.getMessage());
		assertNotNull(anErroneousOrder.getColsErrors().first().getErroneousOrder());
	}

	/**
	 * Method that tests the scenario where after a port move update request (CAN, MCAN, EC etc), the Netcracker sends back a notification
	 * but with a status other than "Succeeded". Only in a succeeded case should COLS update/save the changes on the installed base
	 */
	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/cpeData.xml", "testInputData/bperData_from_dataExport.xml" })
	@TodoFixTest(value = "Should be able to remove DB commit once we have migrated OeOrder to JPA.", developer = Developer.Martin)
	public void updatePortMoveWithNcNotifyingNotSuccess() {

		Long portId = 24914981L;
		Long portId2 = 24914985L;

		// create a test colsAccess object
		ColsAccess colsAccess = this.testData().cols().newColsAccess("1", ColsAccessLegType.LOW_END);

		// Set a canName and canPort
		MscLowEndLeg mscLeg = (MscLowEndLeg) colsAccess.getMscLeg();
		mscLeg.getNePort().setNeName("canName");
		mscLeg.getNePort().setNePort("canPort");

		// Create two MscLayers and add it to the Leg
		MscLayer mscLayer1 = new MscLayer();
		mscLayer1.setMscLayerId("mscLayerId1");
		mscLayer1.setCfsId("mscLayerCfsId1");
		mscLayer1.setPortId(portId);

		MscLayer mscLayer2 = new MscLayer();
		mscLayer2.setMscLayerId("mscLayerId2");
		mscLayer2.setCfsId("mscLayerCfsId2");
		mscLayer2.setPortId(portId2);

		mscLeg.add(mscLayer1);
		mscLeg.add(mscLayer2);

		// Persist the ColsAccess
		ColsAccessRepository colsAccessRepo = ServiceLocatorCdi.cdi(ColsAccessRepository.class, QualifierUtils.COLS_ANNO);
		colsAccess = colsAccessRepo.save(colsAccess);

		Assert.assertNotNull("ColsAccess shouldn't be null", colsAccess);
		Assert.assertNotNull("MscLeg shouldn't be null", colsAccess.getMscLeg());
		Assert.assertNotNull("MscLayer shouldn't be null", colsAccess.getMscLeg().getMscLayers());
		Assert.assertEquals("canName", mscLeg.getNePort().getNeName());
		Assert.assertEquals("canPort", mscLeg.getNePort().getNePort());

		Order colsPortMoveNbOrder = this.loadColsOrder("UpdatePortMoveCanMcanNbOrder.xml");

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(colsPortMoveNbOrder);

		OrderX order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", this.getModifiedSetter()));

		// check State
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());

		// Check if the correct notification was sent back to Caller Application once the order was submitted & started at COLS
		this.assertLastNotificationXml("ColsServiceIT_portMoveCanMcan_inProgress_notification.xml");

		debug().commitToDb();

		// This should have triggered the cols order and should have invoked the action class for the business port that send the order to
		// the NC
		// Simulate the notification from the NC - it calls the ColsService.callService() to Notify the status back to COLS
		ColsRequest colsPortMoveNbNotify = this.loadColsRequest("UpdateBusinessPortMoveNbNotify_withStatusNonSucceeded.xml");
		new ColsService().callService(colsPortMoveNbNotify);

		// reload data
		colsAccess = colsAccessRepo.findByColsAccessId(colsAccess.getColsAccessId());

		// This should have updated the canName and canPort on the installed base with the new ones from the NC
		Assert.assertEquals("canName", ((MscLowEndLeg) colsAccess.getMscLeg()).getNePort().getNeName());
		// value should not be updated because the status was not as "succeeded" from NC
		Assert.assertEquals("canPort", ((MscLowEndLeg) colsAccess.getMscLeg()).getNePort().getNePort());

		// reprocess the order
		// reprocessOrder(order.getOrderId());

		// Since the value was not successfully updated, the order should not yet be finished
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
	}

	@Test
	@DataSet(value = "/masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/cpeData.xml", "testInputData/bperData_from_dataExport.xml" })
	public void canUpdateColsAccessCPE_ChangingCPEModel() throws ObjectNotFoundException, BusinessException {
		ColsAccessServicesIT.getInstance(this).test_CreateColsAccessHE_Sip_Mediant(null, null);

		// *********** CREATE P2P Net ********************************************************************************************* /
		Long portId = 24914981L;
		new OoeOrderColsFacadeIT().mockNonExistingVLanServiceId();

		String newColsOrderXml = new IOUtil().loadTextFromUrl(JuUrl.resourceRelativeTo("ColsService_newColsP2P_dualHome.xml",
				this.getClass()));
		Order newColsP2POrder = XmlUtils.marshaller().unmarshal(newColsOrderXml, Order.class);

		new ColsService().submitOrder(newColsP2POrder);

		OrderX order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003", this.getModifiedSetter()));

		OrderDetails od = order.process();
		Assert.assertFalse(od.isHasErrors());

		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());

		OrderType submittedOrder = this.pollLastRequest(OrderType.class);
		new LcsNotificationHandler().simulateNotification(submittedOrder);

		Assert.assertEquals(OrderState.FINISHED, order.getState());

		// *********** EXTEND P2P Net ********************************************************************************************* /

		String extendP2PNetOrderXml = new IOUtil().loadTextFromUrl(JuUrl.resourceRelativeTo("ColsService_extendColsP2PNet_sipMediant.xml",
				this.getClass()));
		Order extendNetOrder = XmlUtils.marshaller().unmarshal(extendP2PNetOrderXml, Order.class);

		List<ColsAccess> tmp = ServiceLocatorCdi.cdi(ColsAccessService.class).searchAll();
		ColsAccess colsAccess = ServiceLocatorCdi.cdi(ColsAccessService.class).searchByColsAccessId(tmp.get(0).getColsAccessId());
		this.testData().cols().newColsCpeModel(colsAccess.getLogicalPhysicalCpe().getCpe().getCpeModel(), colsAccess.getCpeOption());

		AttributeType mediantAttributes = setMediantPortNameAttributes(extendNetOrder);
		extendNetOrder.getOrderItem().get(0).setAttributes(mediantAttributes);

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(extendNetOrder);

		order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000004", this.getModifiedSetter()));

		ColsAccessService colsAccessService = ServiceLocatorCdi.cdi(ColsAccessService.class);
		ColsAccess aColsAccess = colsAccessService.searchByColsAccessId("COL:ACC:1");
		SendOeColsOrder sendOeColsOrder = ServiceLocatorCdi.cdi(SendOeColsOrder.class);
		sendOeColsOrder.updateCpe(aColsAccess, "dummyLanPortName", "dummyLanPortPluggable", new Date(), null, getMetaData());

		// *********** UPDATE COLSACCESS CPE ***************************************************************************************** /

		OrderX orderUpdate = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005", this.getModifiedSetter()));
		OrderX assignCpeOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.001", getModifiedSetter()));
		OrderX assignCpeMaterialsOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.002", getModifiedSetter()));
		OrderX assignInstallRfsDateOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.003", getModifiedSetter()));
		OrderX cpeInstallationOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.004", getModifiedSetter()));
		OrderX colAccessCopeOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.005", getModifiedSetter()));
		OrderX logicalPhysicalCpeCopeOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.006", getModifiedSetter()));
		OrderX cpeInstallationDoneOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.007", getModifiedSetter()));
		OrderX retrivalNetCrakerOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.008", getModifiedSetter()));

		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, assignCpeOrder.getState());
		Assert.assertEquals(OrderState.WAITING, assignCpeMaterialsOrder.getState());
		Assert.assertEquals(OrderState.WAITING, assignInstallRfsDateOrder.getState());
		Assert.assertEquals(OrderState.WAITING, cpeInstallationOrder.getState());
		Assert.assertEquals(OrderState.WAITING, cpeInstallationDoneOrder.getState());
		Assert.assertEquals(OrderState.WAITING, colAccessCopeOrder.getState());
		Assert.assertEquals(OrderState.WAITING, logicalPhysicalCpeCopeOrder.getState());
		Assert.assertEquals(OrderState.WAITING, retrivalNetCrakerOrder.getState());

		od = orderUpdate.process();
		Assert.assertFalse(od.isHasErrors());

		// Simulate that the cpe has been manually assigned.
		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrder = nbOrderService.findByOrderId("EOE-0000005");
		Assert.assertNotNull(nbOrder);

		/********************** ASSIGN CPE ****************************/

		ColsResponseAccessUpdate updateColsAccessResponses = ((ColsResponseAccessUpdate) nbOrder.getColsProcess().getResponse());
		ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).mockSetCpeData("EOE-0000005.001", "CpeModel-2", "cpeWanPortName",
				"cpeWanPortNumber", "cpeLanPortPluggable", "cpeLanPortName", "comment", getMetaData());
		ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).setCpeDataDone("EOE-0000005.001", getMetaData());

		/*************************************************************/

		Assert.assertEquals(OrderState.FINISHED, assignCpeOrder.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, assignCpeMaterialsOrder.getState());
		Assert.assertEquals(OrderState.WAITING, cpeInstallationOrder.getState());
		Assert.assertEquals(OrderState.WAITING, cpeInstallationDoneOrder.getState());
		Assert.assertEquals(OrderState.WAITING, colAccessCopeOrder.getState());
		Assert.assertEquals(OrderState.WAITING, logicalPhysicalCpeCopeOrder.getState());
		Assert.assertEquals(OrderState.WAITING, retrivalNetCrakerOrder.getState());

		// Assign Material and RfsDate
		ColsRequestAccessUpdate aRequest = ((ColsRequestAccessUpdate) nbOrder.getColsProcess().getRequest());
		setInstallRfsDate(orderUpdate, aRequest.getRfsDate(), "");
		updateColsAccessResponses.getTaskStatus().setCpeMaterialDone(true);
		updateColsAccessResponses.getTaskStatus().setRfsDateDone(true);
		ServiceLocatorCdi.cdi(UpdateColsAccessProcessService.class).update((ColsProcessAccessUpdate) nbOrder.getColsProcess());

		// The cpe installation finishes immediatelly because the simulation of wfm answers automatically.
		Assert.assertEquals(OrderState.FINISHED, assignCpeOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, assignCpeMaterialsOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, cpeInstallationOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, colAccessCopeOrder.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, logicalPhysicalCpeCopeOrder.getState());
		Assert.assertEquals(OrderState.WAITING, cpeInstallationDoneOrder.getState());
		Assert.assertEquals(OrderState.WAITING, retrivalNetCrakerOrder.getState());

		// Simulate WFM Response
		int wfmId = Integer.valueOf(((ColsResponseAccessUpdate) nbOrder.getColsProcess().getResponse()).getWfmId());
		ServiceLocatorCdi.cdi(ColsWfmSimulator.class).simulateResponse(wfmId, ColsWfmSimulator.CLOSED);

		// Check sent order
		this.assertion().cope().loggedXmlEqualsResource("ColsServiceIT_updateColsAccessCPE_ChangingCPEModel.xml", "EOE-0000005.005");

		// Simulate CoPE Notification
		new LcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		// Update ColsAccess is finished
		Assert.assertEquals(OrderState.FINISHED, assignCpeOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, assignCpeMaterialsOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, cpeInstallationOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, cpeInstallationDoneOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, colAccessCopeOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, logicalPhysicalCpeCopeOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, retrivalNetCrakerOrder.getState());

		Assert.assertEquals("CpeModel-2", updateColsAccessResponses.getCpe().getCpeModel());

		// ******Continue with the EXTEND P2P Net ***************************************************************************** //

		List<MediantPortName> mediantPortNames = new ArrayList<MediantPortName>();
		MediantPortName mediantPortName = new MediantPortName();
		mediantPortName.setPosition(1);
		mediantPortName.setMediantPortName("mediantPortName-1");
		mediantPortNames.add(mediantPortName);
		mediantPortName = new MediantPortName();
		mediantPortName.setPosition(2);
		mediantPortName.setMediantPortName("mediantPortName-2");
		mediantPortNames.add(mediantPortName);
		setMediantPortNames("EOE-0000004", mediantPortNames);

		// Get the order that was implicitly created by the ColsService
		OrderX assignBperOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000004.003", this.getModifiedSetter()));
		OrderX extendOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000004.005", this.getModifiedSetter()));

		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());

		// COLS-680: This simulates NEXT STEP without all the complex validations
		mockCpeLanPortNameDone("EOE-0000004.001");

		// The BPER assign order must be in state IN_PROGRESS to enter BPER data
		Assert.assertEquals(OrderState.IN_PROGRESS, assignBperOrder.getState());

		// Assign BPER data
		ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).assignBper(extendOrder.getOrderId(), portId, getMetaData(), false);

		// The BPER assign order must be in state FINISHED
		Assert.assertEquals(OrderState.FINISHED, assignBperOrder.getState());

		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_extendP2PNet_copeOrder_Layer2_sipMediant.xml");

		// Simulate notification
		submittedOrder = this.pollLastRequest(OrderType.class);
		new LcsNotificationHandler().simulateNotification(submittedOrder);

		// Check sent order
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_extendP2PNet_copeOrder_sipMediant.xml");

		// Make sure order is not finished yet
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());

		// Simulate notification
		submittedOrder = this.pollLastRequest(OrderType.class);
		new LcsNotificationHandler().simulateNotification(submittedOrder);

		// Check notification XML
		this.assertLastNotificationXml("ColsServiceIT_extendP2PNet_notification_sipMediant.xml");

		// Make sure the order is finished now
		Assert.assertEquals(OrderState.FINISHED, order.getState());

		List<L3ServiceConnectivity> connectivitesList = ServiceLocatorCdi.cdi(L3ServiceConnectivityRepository.class,
				QualifierUtils.COLS_ANNO).findAll();
		Assert.assertEquals(1, connectivitesList.size());
		Assert.assertEquals(Integer.valueOf(50), connectivitesList.get(0).getChannels());
		Assert.assertEquals("yes", connectivitesList.get(0).getOptionMediant());
		Assert.assertEquals(2, connectivitesList.get(0).getMediantPorts().size());
		Assert.assertEquals("CpeModel-2", updateColsAccessResponses.getCpe().getCpeModel());

		// Check ColsUpdateHistory attributes
		ColsUpdateHistoryRepository updateHistoryRepo = ServiceLocatorCdi.cdi(ColsUpdateHistoryRepository.class, QualifierUtils.COLS_ANNO);
		List<ColsUpdateHistory> updates = updateHistoryRepo.findByColsEntityId(colsAccess.getColsAccessId());
		assertEquals(1, updates.size());
		SortedSet<AttributeUpdate> attributes = updates.get(0).getAttributes();
		assertEquals(3, attributes.size());
		assertEquals("COL:ACC:1", updates.get(0).getColsEntityId());
		assertEquals("UnitTest", updates.get(0).getChangeUser());
		assertEquals("CpeLanPortName1", attributes.first().getAttributeName());
		assertEquals("cpeLanPortName1", attributes.first().getOldValue());
		assertEquals("cpeLanPortName", attributes.first().getNewValue());
		assertEquals("CpeModel", attributes.last().getAttributeName());
		assertEquals("CpeModel-1", attributes.last().getOldValue());
		assertEquals("CpeModel-2", attributes.last().getNewValue());
	}

	@Test
	@DataSet(value = "/masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/cpeData.xml", "testInputData/bperData_from_dataExport.xml" })
	public void canUpdateColsAccessCPE_NOTChangingCPEModel() throws ObjectNotFoundException, BusinessException {
		ColsAccessServicesIT.getInstance(this).test_CreateColsAccessHE_Sip_Mediant(null, null);

		// *********** CREATE P2P Net ********************************************************************************************* /
		Long portId = 24914981L;
		new OoeOrderColsFacadeIT().mockNonExistingVLanServiceId();

		String newColsOrderXml = new IOUtil().loadTextFromUrl(JuUrl.resourceRelativeTo("ColsService_newColsP2P_dualHome.xml",
				this.getClass()));
		Order newColsP2POrder = XmlUtils.marshaller().unmarshal(newColsOrderXml, Order.class);

		new ColsService().submitOrder(newColsP2POrder);

		OrderX order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000003", this.getModifiedSetter()));

		OrderDetails od = order.process();
		Assert.assertFalse(od.isHasErrors());

		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());

		OrderType submittedOrder = this.pollLastRequest(OrderType.class);
		new LcsNotificationHandler().simulateNotification(submittedOrder);

		Assert.assertEquals(OrderState.FINISHED, order.getState());

		// *********** EXTEND P2P Net ********************************************************************************************* /

		String extendP2PNetOrderXml = new IOUtil().loadTextFromUrl(JuUrl.resourceRelativeTo("ColsService_extendColsP2PNet_sipMediant.xml",
				this.getClass()));
		Order extendNetOrder = XmlUtils.marshaller().unmarshal(extendP2PNetOrderXml, Order.class);

		List<ColsAccess> tmp = ServiceLocatorCdi.cdi(ColsAccessService.class).searchAll();
		ColsAccess colsAccess = ServiceLocatorCdi.cdi(ColsAccessService.class).searchByColsAccessId(tmp.get(0).getColsAccessId());
		this.testData().cols().newColsCpeModel(colsAccess.getLogicalPhysicalCpe().getCpe().getCpeModel(), colsAccess.getCpeOption());

		AttributeType mediantAttributes = setMediantPortNameAttributes(extendNetOrder);
		extendNetOrder.getOrderItem().get(0).setAttributes(mediantAttributes);

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(extendNetOrder);

		order = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000004", this.getModifiedSetter()));

		ColsAccessService colsAccessService = ServiceLocatorCdi.cdi(ColsAccessService.class);
		ColsAccess aColsAccess = colsAccessService.searchByColsAccessId("COL:ACC:1");
		SendOeColsOrder sendOeColsOrder = ServiceLocatorCdi.cdi(SendOeColsOrder.class);
		sendOeColsOrder.updateCpe(aColsAccess, "dummyLanPortName", "dummyLanPortPluggable", new Date(), null, getMetaData());

		// *********** UPDATE COLSACCESS CPE ***************************************************************************************** /

		OrderX orderUpdate = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005", this.getModifiedSetter()));
		OrderX assignCpeOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.001", getModifiedSetter()));
		OrderX assignCpeMaterialsOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.002", getModifiedSetter()));
		OrderX assignInstallRfsDateOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.003", getModifiedSetter()));
		OrderX cpeInstallationOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.004", getModifiedSetter()));
		OrderX colAccessCopeOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.005", getModifiedSetter()));
		OrderX logicalPhysicalCpeCopeOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.006", getModifiedSetter()));
		OrderX cpeInstallationDoneOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.007", getModifiedSetter()));
		OrderX retrivalNetCrakerOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000005.008", getModifiedSetter()));

		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, assignCpeOrder.getState());
		Assert.assertEquals(OrderState.WAITING, assignCpeMaterialsOrder.getState());
		Assert.assertEquals(OrderState.WAITING, assignInstallRfsDateOrder.getState());
		Assert.assertEquals(OrderState.WAITING, cpeInstallationOrder.getState());
		Assert.assertEquals(OrderState.WAITING, cpeInstallationDoneOrder.getState());
		Assert.assertEquals(OrderState.WAITING, colAccessCopeOrder.getState());
		Assert.assertEquals(OrderState.WAITING, logicalPhysicalCpeCopeOrder.getState());
		Assert.assertEquals(OrderState.WAITING, retrivalNetCrakerOrder.getState());

		od = orderUpdate.process();
		Assert.assertFalse(od.isHasErrors());

		// Simulate that the cpe has been manually assigned.
		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrder = nbOrderService.findByOrderId("EOE-0000005");
		Assert.assertNotNull(nbOrder);

		/********************** ASSIGN CPE ****************************/

		ColsResponseAccessUpdate updateColsAccessResponses = ((ColsResponseAccessUpdate) nbOrder.getColsProcess().getResponse());
		ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).mockSetCpeData("EOE-0000005.001", "CpeModel-1", "cpeWanPortName",
				"cpeWanPortNumber", "cpeLanPortPluggable-1", "cpeLanPortName1", "comment", getMetaData());
		ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).setCpeDataDone("EOE-0000005.001", getMetaData());

		/*************************************************************/

		Assert.assertEquals(OrderState.FINISHED, assignCpeOrder.getState());
		Assert.assertEquals(OrderState.IN_PROGRESS, assignCpeMaterialsOrder.getState());
		Assert.assertEquals(OrderState.WAITING, cpeInstallationOrder.getState());
		Assert.assertEquals(OrderState.WAITING, cpeInstallationDoneOrder.getState());
		Assert.assertEquals(OrderState.SKIPPED, colAccessCopeOrder.getState());
		Assert.assertEquals(OrderState.SKIPPED, logicalPhysicalCpeCopeOrder.getState());
		Assert.assertEquals(OrderState.WAITING, retrivalNetCrakerOrder.getState());

		// Assign Material and RfsDate
		ColsRequestAccessUpdate aRequest = ((ColsRequestAccessUpdate) nbOrder.getColsProcess().getRequest());
		setInstallRfsDate(orderUpdate, aRequest.getRfsDate(), "");
		updateColsAccessResponses.getTaskStatus().setCpeMaterialDone(true);
		updateColsAccessResponses.getTaskStatus().setRfsDateDone(true);
		ServiceLocatorCdi.cdi(UpdateColsAccessProcessService.class).update((ColsProcessAccessUpdate) nbOrder.getColsProcess());

		// Simulate WFM Response
		int wfmId = Integer.valueOf(((ColsResponseAccessUpdate) nbOrder.getColsProcess().getResponse()).getWfmId());
		ServiceLocatorCdi.cdi(ColsWfmSimulator.class).simulateResponse(wfmId, ColsWfmSimulator.CLOSED);

		// The cpe installation finishes immediatelly because the simulation of wfm answers automatically.
		Assert.assertEquals(OrderState.FINISHED, assignCpeOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, assignCpeMaterialsOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, cpeInstallationOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, cpeInstallationDoneOrder.getState());
		Assert.assertEquals(OrderState.SKIPPED, colAccessCopeOrder.getState());
		Assert.assertEquals(OrderState.SKIPPED, logicalPhysicalCpeCopeOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, retrivalNetCrakerOrder.getState());

		// We are not sending cope notification
		// this.assertion().cope().loggedXmlEqualsResource("ColsServiceIT_updateColsAccessCPE_NOTChangingCPEModel.xml", "EOE-0000005.006");
		// new LcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		Assert.assertEquals("CpeModel-1", updateColsAccessResponses.getCpe().getCpeModel());

		// ******Continue with the EXTEND P2P Net ***************************************************************************** //

		List<MediantPortName> mediantPortNames = new ArrayList<MediantPortName>();
		MediantPortName mediantPortName = new MediantPortName();
		mediantPortName.setPosition(1);
		mediantPortName.setMediantPortName("mediantPortName-1");
		mediantPortNames.add(mediantPortName);
		mediantPortName = new MediantPortName();
		mediantPortName.setPosition(2);
		mediantPortName.setMediantPortName("mediantPortName-2");
		mediantPortNames.add(mediantPortName);
		setMediantPortNames("EOE-0000004", mediantPortNames);

		// Get the order that was implicitly created by the ColsService
		OrderX assignBperOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000004.003", this.getModifiedSetter()));
		OrderX extendOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000004.005", this.getModifiedSetter()));

		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());

		// COLS-680: This simulates NEXT STEP without all the complex validations
		mockCpeLanPortNameDone("EOE-0000004.001");

		// The BPER assign order must be in state IN_PROGRESS to enter BPER data
		Assert.assertEquals(OrderState.IN_PROGRESS, assignBperOrder.getState());

		// Assign BPER data
		ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).assignBper(extendOrder.getOrderId(), portId, getMetaData(), false);

		// The BPER assign order must be in state FINISHED
		Assert.assertEquals(OrderState.FINISHED, assignBperOrder.getState());

		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_extendP2PNet_copeOrder_Layer2_sipMediant.xml");

		// Simulate notification
		submittedOrder = this.pollLastRequest(OrderType.class);
		new LcsNotificationHandler().simulateNotification(submittedOrder);

		// Check sent order
		this.assertion().cope().lastSentOrderEqualsResource("ColsServiceIT_extendP2PNet_copeOrder_sipMediant.xml");

		// Make sure order is not finished yet
		Assert.assertEquals(OrderState.IN_PROGRESS, order.getState());

		// Simulate notification
		submittedOrder = this.pollLastRequest(OrderType.class);
		new LcsNotificationHandler().simulateNotification(submittedOrder);

		// Check notification XML
		this.assertLastNotificationXml("ColsServiceIT_extendP2PNet_notification_sipMediant.xml");

		// Make sure the order is finished now
		Assert.assertEquals(OrderState.FINISHED, order.getState());

		List<L3ServiceConnectivity> connectivitesList = ServiceLocatorCdi.cdi(L3ServiceConnectivityRepository.class,
				QualifierUtils.COLS_ANNO).findAll();
		Assert.assertEquals(1, connectivitesList.size());
		Assert.assertEquals(Integer.valueOf(50), connectivitesList.get(0).getChannels());
		Assert.assertEquals("yes", connectivitesList.get(0).getOptionMediant());
		Assert.assertEquals(2, connectivitesList.get(0).getMediantPorts().size());
		Assert.assertEquals("CpeModel-1", updateColsAccessResponses.getCpe().getCpeModel());
		Assert.assertEquals("CpeModel-1", connectivitesList.get(0).getL3ServiceConnectivityAccess().getColsAccess().getLogicalPhysicalCpe()
				.getCpe().getCpeModel());

		// Check ColsUpdateHistory attributes
		ColsUpdateHistoryRepository updateHistoryRepo = ServiceLocatorCdi.cdi(ColsUpdateHistoryRepository.class, QualifierUtils.COLS_ANNO);
		List<ColsUpdateHistory> updates = updateHistoryRepo.findByColsEntityId(colsAccess.getColsAccessId());
		assertEquals(1, updates.size());
		SortedSet<AttributeUpdate> attributes = updates.get(0).getAttributes();
		assertEquals(3, attributes.size());
		assertEquals("COL:ACC:1", updates.get(0).getColsEntityId());
		assertEquals("UnitTest", updates.get(0).getChangeUser());
		assertEquals("CpeLanPortPluggable1", attributes.first().getAttributeName());
		assertEquals("cpeLanPortPluggable1", attributes.first().getOldValue());
		assertEquals("cpeLanPortPluggable-1", attributes.first().getNewValue());
		assertEquals("CpeWanPortName", attributes.last().getAttributeName());
		assertEquals("CpeWanPortName-1", attributes.last().getOldValue());
		assertEquals("cpeWanPortName", attributes.last().getNewValue());
	}

	@Test
	public void given_ColsLeadTime_when_suborderIsDeleteColsAccess_then_RfsDateIsCorrect() throws Exception {
		ColsLeadTimeRepository colsLeadTimeRepository = ServiceLocatorCdi.cdi(ColsLeadTimeRepository.class, QualifierUtils.COLS_ANNO);
		CopeLeadTimeCalc copeLeadTimeCalc = new CopeLeadTimeCalc(colsLeadTimeRepository);
		// Given
		Long typeId = Constants.COLS_ORDER_N20_DELETE;
		// Create the LeadTime
		createColsLeadTimes();
		// When
		Date rfsDate = copeLeadTimeCalc.getRfsDateWithLeadTimeForSuborder(
				JuStringUtils.DATE_FORMAT_SECONDS.parse("01.07.2015 00:00:00"), typeId);
		// Then
		assertEquals(JuStringUtils.DATE_FORMAT_SECONDS.parse("04.07.2015 00:00:00"), rfsDate);
	}

	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/cpeData.xml" })
	public void canAssignCPEMaterials_to_ColsAccessHE() throws Exception {
		String cpeModelName = "Cisco R2";
		String cpeOption = "HighEnd";
		String cpeLanPortName1 = "GigabitEthernet0/0";
		String cpeLanPortPluggable1 = "100Base-TX";
		String cpeLanPortName2 = "GigabitEthernet0/1";
		String cpeLanPortPluggable2 = "1000Base-TX";
		String cpeLanPortName3 = "GigabitEthernet0/2";
		String wanLanPortName = "GigabitEthernet0/1";
		String materialDetail1 = "MaterialDetail-1";
		String materialDetail2 = "MaterialDetail-2";
		String materialDetail3 = "MaterialDetail-3";
		String materialDetail4 = "MaterialDetail-4";
		String material1 = "Material-1";
		String material2 = "Material-2";
		Long quantity1 = 10L;
		Long quantity2 = 20L;

		// ***** CREATE COLSACCESS ******* //
		String newColsAccessOrderXml = new IOUtil().loadTextFromUrl(JuUrl.resourceRelativeTo(
				"ColsService_colsAccessHE_manualChangeCpe.xml",
				ColsServiceCommonBehaviorTest.class));
		Order newColsOrder = XmlUtils.marshaller().unmarshal(newColsAccessOrderXml, Order.class);

		new ColsService().submitOrder(newColsOrder);

		OrderX preOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", this.getModifiedSetter()));

		OrderX s33PreOrderTibcoOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_S33_PRE_ORDER.getType());

		OrderDetails od = preOrder.process();
		Assert.assertFalse(od.isHasErrors());

		CpeModelTestdataCreator
				.setupMatchCpeLanPortAndModel(cpeLanPortName1, cpeModelName, cpeOption, false, false, null, null);
		CpeModelTestdataCreator
				.setupMatchCpeLanPortAndModel(cpeLanPortName2, cpeModelName, cpeOption, false, false, null, null);
		CpeModelTestdataCreator
				.setupMatchCpeLanPortAndModel(cpeLanPortName3, cpeModelName, cpeOption, false, false, null, null);

		ColsCpeModel aColsCpeModel = CpeModelTestdataCreator.setupMatchCpeMaterial(cpeModelName, cpeOption, materialDetail1,
				materialDetail2, materialDetail3,
				materialDetail4, material1, material2);

		setCpeDataWithTwoPlug(preOrder, cpeLanPortName1, cpeLanPortPluggable1, cpeLanPortName2, cpeLanPortPluggable2, cpeModelName,
				wanLanPortName, "0/1", null,
				null, "");

		OrderType submittedOrder = this.pollLastRequest(OrderType.class);
		new LcsNotificationHandler().simulateNotification(submittedOrder, NotificationType.SUBMIT_ORDER_ACK);

		ColsResponseAccessCreateHighEnd response = preOrder.getDataObjectNotNull(ColsProcessAccessCreateHighEnd.class, true).getResponse();
		Assert.assertNotNull(response.getCircuitId());

		new LcsNotificationHandler().simulateNotification(submittedOrder);

		ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).confirmPreOrder(s33PreOrderTibcoOrder.getOrderId(), getMetaData());

		OrderX n20ConfirmOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002", getModifiedSetter()));
		Assert.assertNotNull(n20ConfirmOrder);

		setCpeOrderDone(n20ConfirmOrder);

		ServiceLocatorCdi.cdi(ColsOrderAlarmActivationService.class).activateAlarm(n20ConfirmOrder.getOrderId());

		OrderX assignCpeMaterialOrder = n20ConfirmOrder.getSubOrderX(ColsOrderType.COLS_ORDER_ASSIGN_CPE_MATERIAL.getType());
		Assert.assertNotNull(assignCpeMaterialOrder);

		// Assign Materials
		SortedSet<CpeMaterialUse> cpeMaterialUseList = new TreeSet<CpeMaterialUse>();
		for (CpeMaterial aCpeMaterial : aColsCpeModel.getCpeMaterialList()) {
			CpeMaterialUse aCpeMaterialUse = new CpeMaterialUse();
			aCpeMaterialUse.setName(aCpeMaterial.getName());
			aCpeMaterialUse.setDescription(aCpeMaterial.getDescription());
			aCpeMaterialUse.setSapId(aCpeMaterial.getSapId());
			if (aCpeMaterial.getDescription().contains("1")) {
				aCpeMaterialUse.setQuantity(quantity1);
			} else {
				aCpeMaterialUse.setQuantity(quantity2);
			}
			cpeMaterialUseList.add(aCpeMaterialUse);
		}
		EoeOrderServiceCols eoeOrderServiceCols = ServiceLocatorCdi.cdi(EoeOrderServiceCols.class);
		eoeOrderServiceCols.setCpeMaterialUseList(assignCpeMaterialOrder.getOrderId(), cpeMaterialUseList, getMetaData());
		eoeOrderServiceCols.setCpeMaterialDone(assignCpeMaterialOrder.getOrderId(), getMetaData());

		setCreateColsAccessOrderTasks(n20ConfirmOrder, getMetaData());

		// Check create ColsNorthboundOrder.
		ColsNorthboundOrderService nbOrderService = ServiceLocatorCdi.cdi(ColsNorthboundOrderService.class);
		ColsNorthboundOrder nbOrder = nbOrderService.findByOrderId("EOE-0000002");
		Assert.assertNotNull(nbOrder);
		ColsProcessAccessConfirm process = (ColsProcessAccessConfirm) nbOrder.getColsProcess();

		// Simulate WFM Response
		int wfmId = Integer.valueOf(process.getResponse().getWfmId());
		ServiceLocatorCdi.cdi(ColsWfmSimulator.class).simulateResponse(wfmId, ColsWfmSimulator.CLOSED);

		new LcsNotificationHandler().simulateNotification(this.pollLastRequest(OrderType.class));

		ServiceLocatorCdi.cdi(ColsOrderAlarmActivationService.class).activateAlarm(n20ConfirmOrder.getOrderId());

		Assert.assertEquals(OrderState.FINISHED, n20ConfirmOrder.getState());
		Assert.assertEquals(OrderState.FINISHED, preOrder.getState());

		ColsAccessService colsAccessService = ServiceLocatorCdi.cdi(ColsAccessService.class);
		List<ColsAccess> accesses = colsAccessService.searchAll();
		assertEquals(1, accesses.size());

		ColsAccess aColsAccess = accesses.get(0);

		assertEquals("ServiceIdLogicalPhysicalCpe", aColsAccess.getLogicalPhysicalCpe().getCfsIdLogicalPhysicalCpe());

		Cpe aCpe = aColsAccess.getLogicalPhysicalCpe().getCpe();

		assertEquals(cpeModelName, aCpe.getCpeModel());
		assertEquals(2, aCpe.getCpeMaterialUseList().size());
		assertEquals(material1, aCpe.getCpeMaterialUseList().first().getName());
		assertEquals(quantity1, aCpe.getCpeMaterialUseList().first().getQuantity());
		assertEquals(material2, aCpe.getCpeMaterialUseList().last().getName());
		assertEquals(quantity2, aCpe.getCpeMaterialUseList().last().getQuantity());

	}

	@Test
	@DataSet(value = "masterData/emptyDb_colsAll.xml"
			, inserts = { "testInputData/cpeData.xml", "testInputData/testUser.xml" })
	public void given_colsAccessHeOrder_when_baskaltaskAndBaskalEventAreSet_then_notifyCFU() throws Exception {

		// Given
		Order newColsOrder = OrderCreation.createNewAccessOrder();

		// Hit the northbound COLS interface with the order
		new ColsService().submitOrder(newColsOrder);

		// Get the order that was implicitly created by the ColsService
		OrderX preOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000001", this.getModifiedSetter()));

		// Get SubOrders we will make checks on
		OrderX s33PreOrderTibcoOrder = preOrder.getSubOrderX(ColsOrderType.COLS_ORDER_S33_PRE_ORDER.getType());

		// Process the order to make sure it's processed without errors
		OrderDetails od = preOrder.process();
		Assert.assertFalse(od.isHasErrors());

		// Simulate the GUI Assign of CPE Data. GUI calls same service as below:
		setCpeDataWithOnePlug(preOrder, false);

		// Simulate notification from SouthBound
		OrderType submittedOrder = this.pollLastRequest(OrderType.class);

		// First, CoPE will send an intermediate notification of type SubmitOrderAck with status succeeded,
		// containing one OrderLineItem result for the PO_MSC_HIGH_END_LEG with status succeeded, containing the attribute
		// circuitId.
		// As soon as the circuitId is known, we need to set it on the responses object and send a notification to CFU.

		new LcsNotificationHandler().simulateNotification(submittedOrder, NotificationType.SUBMIT_ORDER_ACK);

		new LcsNotificationHandler().simulateNotification(submittedOrder);

		// Confirm this order through the EoeOrderServiceCols
		ServiceLocatorCdi.cdi(EoeOrderServiceCols.class).confirmPreOrder(s33PreOrderTibcoOrder.getOrderId(), getMetaData());

		OrderX n20ConfirmOrder = EoeOrderUtils.asX(EoeOrderUtils.findOrder("EOE-0000002", getModifiedSetter()));
		Assert.assertNotNull(n20ConfirmOrder);

		ServiceLocatorCdi.cdi(OeOrderRepository.class).findByOrderId("EOE-0000002.005").get(0)
				.setStatusId(Constants.OE_ORDER_STATE_SKIPPED);

		// Now try to send intermediate information
		Notification notification = new Notification();
		GregorianCalendar cal = new GregorianCalendar();
		cal.setTime(EswDateUtils.getNow());

		notification.setOrderId("EOE-0000001");
		notification.setDateTime(DatatypeFactory.newInstance().newXMLGregorianCalendar(cal));
		com.swisscom.esw.cols.intermediateinfo.ws.AttributeType xxx = new com.swisscom.esw.cols.intermediateinfo.ws.AttributeType();
		notification.setAttributes(xxx);
		addNotificationAttribute(notification, "baskaltask", "EQINS");

		// if baskaltask value "EQINS" and baskalEvent has the value in ("PCU", "PTU")
		// then notify CFU the attribute CpeInstallationDate = baskalStatusTime
		addNotificationAttribute(notification, "baskalevent", "PCU");

		// When
		new IntermediateInformationService().intermediateInformation(notification);
		// Check CFU notification XML
		this.assertLastNotificationXml("ColsServiceIT_PCU_PTU_notifyCFU.xml");

		addNotificationAttribute(notification, "baskalevent", "PTU");
		new IntermediateInformationService().intermediateInformation(notification);

		// Then
		// Check CFU notification XML
		this.assertLastNotificationXml("ColsServiceIT_PCU_PTU_notifyCFU.xml");

		// if baskaltask value is "EQINS" and baskalEvent value is "PTS" then notify CFU the attribute
		// "WfmDoneDate" = BaskalStatusTime
		addNotificationAttribute(notification, "baskalevent", "PTS");
		new IntermediateInformationService().intermediateInformation(notification);
		// Check CFU notification XML
		this.assertLastNotificationXml("ColsServiceIT_PTS_notifyCFU.xml");
	}

	private void addNotificationAttribute(Notification notification, String name, String value) {
		com.swisscom.esw.cols.intermediateinfo.ws.AttributeType.Attribute attr = new com.swisscom.esw.cols.intermediateinfo.ws.AttributeType.Attribute();
		attr.setName(name);
		attr.setValue(value);
		notification.getAttributes().getAttribute().add(attr);
	}

	/**
	 * Add to list of MediantPortNames to the order
	 * 
	 * @param extendNetOrder
	 * @return
	 */
	private AttributeType setMediantPortNameAttributes(Order extendNetOrder) {
		AttributeType.Attribute mediantPortNameAttribute1 = new AttributeType.Attribute();
		mediantPortNameAttribute1.setName("MediantPortName1");
		mediantPortNameAttribute1.setValue("FE1");

		AttributeType.Attribute mediantPortNameAttribute2 = new AttributeType.Attribute();
		mediantPortNameAttribute2.setName("MediantPortName2");
		mediantPortNameAttribute2.setValue("FE2");

		AttributeType attributes = extendNetOrder.getOrderItem().get(0).getAttributes();
		attributes.getAttribute().add(mediantPortNameAttribute1);
		attributes.getAttribute().add(mediantPortNameAttribute2);
		return attributes;
	}
}

