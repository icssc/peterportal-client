import './MenuTile.scss';
import { FC, ReactNode } from 'react';

import DeleteOutlineIcon from '@mui/icons-material/DeleteOutline';
import { IconButton } from '@mui/material';
import UnreadDot from '../../../component/UnreadDot/UnreadDot';

import UnitsContainer from '../CustomUnitsContainer';

export interface MenuTileProps {
  children?: ReactNode;
  title: string;
  units?: number;
  setUnits?: (value: number) => void;
  deleteFn?: () => void;
  /** Additional items to include alongsite the title */
  headerItems?: ReactNode;
  unread?: boolean;
  onClick?: () => void;
}

const MenuTile: FC<MenuTileProps> = ({ children, title, units, setUnits, deleteFn, headerItems, unread, onClick }) => {
  return (
    <div
      className={`menu-tile ${onClick ? 'clickable' : ''}`}
      onClick={onClick}
      role={onClick ? 'button' : undefined}
      tabIndex={onClick ? 0 : undefined}
      onKeyDown={(e) => {
        if (onClick && (e.key === 'Enter' || e.key === ' ')) {
          onClick();
        }
      }}
    >
      <UnreadDot show={unread ?? false} displayFullNewText={true} />
      <div className="tile-info">
        <div className="name">
          {title} {headerItems}
        </div>
        <hr />
        {units !== undefined && (
          <UnitsContainer units={units} setUnits={setUnits} minUnits={0} maxUnits={undefined} source="MenuTile" />
        )}
        {deleteFn && (
          <IconButton className="delete-btn" onClick={deleteFn}>
            <DeleteOutlineIcon />
          </IconButton>
        )}
      </div>
      {children}
    </div>
  );
};

export default MenuTile;
