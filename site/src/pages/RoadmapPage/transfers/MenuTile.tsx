import { CheckLg, PencilSquare, Trash } from 'react-bootstrap-icons';
import { pluralize } from '../../../helpers/util';
import './MenuTile.scss';
import { FC, FormEvent, ReactNode, useState } from 'react';

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
          <button onClick={() => setEditing(true)}>
            <PencilSquare />
          </button>
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
      <button type="submit">
        <CheckLg />
      </button>
    </form>
  );
};

export interface MenuTileProps {
  children?: ReactNode;
  title: string;
  units?: number;
  setUnits?: (value: number) => void;
  deleteFn?: () => void;
  /** Additional items to include alongsite the title */
  headerItems?: ReactNode;
}

const MenuTile: FC<MenuTileProps> = ({ children, title, units, setUnits, deleteFn, headerItems }) => {
  return (
    <div className="menu-tile">
      <div className="tile-info">
        <div className="name">
          {title} {headerItems}
        </div>
        <hr />
        {units !== undefined && <UnitsContainer units={units} setUnits={setUnits} />}
        {deleteFn && (
          <button className="delete-btn" onClick={deleteFn}>
            <Trash />
          </button>
        )}
      </div>
      {children}
    </div>
  );
};

export default MenuTile;
