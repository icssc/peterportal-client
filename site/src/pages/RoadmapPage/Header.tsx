import { FC } from 'react';
import { Button, ButtonGroup } from 'react-bootstrap';
import { pluralize } from '../../helpers/util';
import './Header.scss';
import RoadmapMultiplan from './RoadmapMultiplan';

import SaveAltIcon from '@mui/icons-material/SaveAlt';

interface HeaderProps {
  courseCount: number;
  unitCount: number;
  missingPrerequisites: Set<string>;
  saveRoadmap: () => void;
}

const Header: FC<HeaderProps> = ({ courseCount, unitCount, saveRoadmap }) => {
  return (
    <div className="header">
      <div className="planner-left">
        <RoadmapMultiplan />
        <span id="planner-stats">
          Total: <span id="course-count">{courseCount}</span> course{pluralize(courseCount)},{' '}
          <span id="unit-count">{unitCount}</span> unit{pluralize(unitCount)}
        </span>
      </div>
      <div className="planner-right">
        <ButtonGroup>
          <Button variant={'light'} className={'header-btn'} onClick={saveRoadmap}>
            Save
            <SaveAltIcon style={{ marginLeft: '5px' }} />
          </Button>
        </ButtonGroup>
      </div>
    </div>
  );
};

export default Header;
