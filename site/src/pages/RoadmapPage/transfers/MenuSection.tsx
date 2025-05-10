import './MenuSection.scss';
import { FC, ReactNode } from 'react';

export const SectionDescription: FC<{ children: ReactNode }> = ({ children }) => {
  return <p className="section-description">{children}</p>;
};

interface MenuSectionProps {
  title: string;
  children?: ReactNode;
}

const MenuSection: FC<MenuSectionProps> = ({ title, children }) => {
  return (
    <div className="transfer-credits-section">
      <h4>{title}</h4>
      {children}
    </div>
  );
};

export default MenuSection;
