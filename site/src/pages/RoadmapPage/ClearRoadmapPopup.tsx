import { clearPlanner, setShowClearRoadmapPopup } from '../../store/slices/roadmapSlice';
import { Button, Modal } from 'react-bootstrap';
import { useAppDispatch, useAppSelector } from '../../store/hooks';

const ClearRoadmapPopup = () => {
  const dispatch = useAppDispatch();
  const show = useAppSelector((state) => state.roadmap.showClearRoadmapPopup);
  const handleClose = () => dispatch(setShowClearRoadmapPopup(false));
  const currentPlanIndex = useAppSelector((state) => state.roadmap.currentPlanIndex);
  const allPlans = useAppSelector((state) => state.roadmap);
  const { name } = allPlans.plans[currentPlanIndex];

  return (
    <Modal className="clearRoadmap-modal ppc-modal" show={show} onHide={handleClose} centered>
      <Modal.Header closeButton>
        <h2>Delete Roadmap</h2>
      </Modal.Header>
      <Modal.Body>
        <p>Are you sure you want to delete the roadmap "{name}"?</p>
      </Modal.Body>
      <Modal.Footer>
        <Button
          variant="danger"
          onClick={() => {
            dispatch(clearPlanner());
            handleClose();
          }}
        >
          I am sure
        </Button>
      </Modal.Footer>
    </Modal>
  );
};
export default ClearRoadmapPopup;
