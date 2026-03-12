import { FC, useState } from 'react';
import { useSaveRoadmap } from '../../hooks/planner';
import { Button } from '@mui/material';
import SaveIcon from '@mui/icons-material/Save';
import { useAppSelector } from '../../store/hooks';

const SaveButton: FC = () => {
  const { handler: saveRoadmap } = useSaveRoadmap();
  const roadmapLoading = useAppSelector((state) => state.roadmap.roadmapLoading);
  const customCoursesLoaded = useAppSelector((state) => state.customCourses.customCoursesLoaded);

  const [saveInProgress, setSaveInProgress] = useState(false);

  const handleSave = () => {
    setSaveInProgress(true);
    saveRoadmap().finally(() => setSaveInProgress(false));
  };

  return (
    <Button
      className="header-button"
      variant="text"
      size="medium"
      startIcon={<SaveIcon />}
      loading={saveInProgress}
      disabled={roadmapLoading || !customCoursesLoaded}
      onClick={handleSave}
      color="inherit"
    >
      Save
    </Button>
  );
};

export default SaveButton;
