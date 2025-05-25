import { FC } from 'react';
import { pluralize } from '../../helpers/util';
import { UnitsContainer } from './transfers/MenuTile';

const CourseUnits: FC = ({ minUnits, maxUnits }) => {
  if (minUnits == maxUnits) {
    return (
      <span className="units">
        {minUnits} unit{pluralize(maxUnits)}
      </span>
    );
  }

  return <UnitsContainer units={minUnits} setUnits={undefined} />;
};

export default CourseUnits;
