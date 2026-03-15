import { pluralize } from '../../helpers/util';
import './transfers/MenuTile.scss';
import { FC, FormEvent, useState } from 'react';

import CheckIcon from '@mui/icons-material/Check';
import ModeEditIcon from '@mui/icons-material/ModeEdit';
import { IconButton } from '@mui/material';

// placed in roadmap directory for now
interface UnitsContainerProps {
  units: number;
  setUnits?: (value: number) => void;
}
const UnitsContainer: FC<UnitsContainerProps> = ({ units, setUnits }) => {
  const [editing, setEditing] = useState(false);

  if (!editing || !setUnits) {
    return (
      <>
        <p className="units-display">
          {units} {pluralize(units, 'units', 'unit')}
        </p>
        {setUnits && (
          <IconButton onClick={() => setEditing(true)}>
            <ModeEditIcon />
          </IconButton>
        )}
      </>
    );
  }

  const handleSubmit = (event: FormEvent) => {
    event.preventDefault();
    const formData = new FormData(event.target as HTMLFormElement);
    const unitsValue = parseFloat(formData.get('units') as string);
    setUnits(unitsValue);
    setEditing(false);
  };

  return (
    <form onSubmit={handleSubmit}>
      {/* eslint-disable jsx-a11y/no-autofocus */}
      <input
        className="units-input"
        type="number"
        placeholder="Units"
        name="units"
        defaultValue={units}
        min="0"
        step="any"
        autoFocus
      />
      {/* eslint-enable jsx-a11y/no-autofocus */}
      <IconButton type="submit">
        <CheckIcon />
      </IconButton>
    </form>
  );
};

export default UnitsContainer;
