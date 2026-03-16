import { FC } from 'react';

import './Course.scss';
import UnitsContainer from '../UnitsContainer';

/** @todo delete CustomUnits*/
interface CustomUnitsProps {
  unit: number | undefined;
  minUnits: number;
  maxUnits: number;
  onSetVariableUnits: ((units: number | undefined) => void) | undefined;
}

const CustomUnits: FC<CustomUnitsProps> = ({ unit, minUnits, maxUnits, onSetVariableUnits }) => {
  return (
    <>
      <div className="custom-units">
        {<UnitsContainer units={unit} setUnits={onSetVariableUnits} minUnits={minUnits} maxUnits={maxUnits} />}
      </div>
    </>
  );
};

export default CustomUnits;
