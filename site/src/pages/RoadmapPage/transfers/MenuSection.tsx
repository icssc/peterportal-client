import './MenuSection.scss';
import { FC } from 'react';

interface SectionDescriptionProps {
  children: React.ReactNode;
}

export const SectionDescription: FC<SectionDescriptionProps> = ({ children }) => {
  return <p className="section-description">{children}</p>;
};

interface MenuSectionProps {
  title: string;
  children?: React.ReactNode;
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
