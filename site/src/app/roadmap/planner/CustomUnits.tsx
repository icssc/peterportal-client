import React, { FC, useState } from 'react';
import { IconButton, OutlinedInput, ClickAwayListener } from '@mui/material';
import EditIcon from '@mui/icons-material/Edit';

import { pluralize } from '../../../helpers/util';

import './Course.scss';

interface CustomUnitsProps {
  inputUnit: string | undefined;
  setInputUnit: (input: string) => void;
  userChosenUnits: number | undefined;
  displayedUnits: number | undefined;
  defaultUnitsText: string;
  editUnitOpened: boolean;
  setEditUnitOpened: (e: boolean) => void;
  minUnits: number;
  maxUnits: number;
  onSetVariableUnits: ((units: number | undefined) => void) | undefined;
}

const CustomUnits: FC<CustomUnitsProps> = (props) => {
  const {
    inputUnit,
    setInputUnit,
    userChosenUnits,
    displayedUnits,
    defaultUnitsText,
    editUnitOpened,
    setEditUnitOpened,
    minUnits,
    maxUnits,
    onSetVariableUnits,
  } = props;

  const [hasInputError, setHasInputError] = useState(false);

  const handleVariableUnitChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    e.preventDefault();
    const input = e.target.value;
    const units = Number(input);
    setInputUnit(input);

    setHasInputError(Number.isNaN(units) || (input !== '' && (units < minUnits || units > maxUnits)));
  };

  const handleVarUnitSubmit = () => {
    const nextUnits = inputUnit === '' ? undefined : Number(inputUnit);
    setEditUnitOpened(false);
    setHasInputError(false);
    onSetVariableUnits?.(nextUnits);
  };

  // Input box behavior functions
  const handleKeyPress = (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key == 'Enter' && !hasInputError) {
      event.preventDefault();
      handleVarUnitSubmit();
    }
    if (event.key == 'Escape') {
      event.preventDefault();
      setInputUnit(userChosenUnits?.toString() ?? '');
      setEditUnitOpened(false);
    }
  };
  const handleClickAway = () => {
    if (hasInputError) {
      setEditUnitOpened(false);
    } else {
      handleVarUnitSubmit();
    }
  };

  const handleEditClick = () => {
    setInputUnit(userChosenUnits?.toString() ?? '');
    setHasInputError(false);
    setEditUnitOpened(true);
  };

  return (
    <div className="custom-units">
      {editUnitOpened ? (
        <>
          <ClickAwayListener onClickAway={handleClickAway}>
            <OutlinedInput
              className="unit-input"
              placeholder={defaultUnitsText}
              value={inputUnit}
              onChange={handleVariableUnitChange}
              onKeyDown={handleKeyPress}
              size="small"
              fullWidth={false}
              error={hasInputError}
              /* eslint-disable-next-line jsx-a11y/no-autofocus */
              autoFocus
            />
          </ClickAwayListener>

          <span className="units">units</span>
        </>
      ) : (
        <>
          <IconButton className="course-edit-btn">
            <EditIcon onClick={handleEditClick} fontSize="small" />
          </IconButton>

          {displayedUnits ? (
            <span className="units">{`${displayedUnits} unit${pluralize(displayedUnits)}`}</span>
          ) : (
            <span className="units">{defaultUnitsText}</span>
          )}
        </>
      )}
    </div>
  );
};

export default CustomUnits;
