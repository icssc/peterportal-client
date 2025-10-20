import './MenuSection.scss';
import { FC, ReactNode, useState } from 'react';

import { ExpandMore } from '../../../component/ExpandMore/ExpandMore';
import { Collapse } from '@mui/material';

export const SectionDescription: FC<{ children: ReactNode }> = ({ children }) => {
  return <p className="section-description">{children}</p>;
};

interface MenuSectionProps {
  title: string;
  children?: ReactNode;
}

const MenuSection: FC<MenuSectionProps> = ({ title, children }) => {
  const [open, setOpen] = useState(true);
  const toggleExpand = () => setOpen(!open);

  return (
    <div className="transfer-credits-section">
      <button className="header-tab" onClick={toggleExpand}>
        <h4>{title}</h4>
        <ExpandMore expanded={open} onClick={toggleExpand} />
      </button>
      <Collapse in={open} unmountOnExit>
        {children}
      </Collapse>
    </div>
  );
};

export default MenuSection;
