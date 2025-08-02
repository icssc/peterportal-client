import { FC } from 'react';
import './RecentOfferingsTooltip.scss';
import { OverlayTrigger, Popover } from 'react-bootstrap';
import RecentOfferingsTable from '../RecentOfferingsTable/RecentOfferingsTable';

interface RecentOfferingsTooltipProps {
  terms: string[];
}

const RecentOfferingsTooltip: FC<RecentOfferingsTooltipProps> = ({ terms }) => {
  if (terms.length === 0) return null;

  const prevYear = new Date().getMonth() > 9 ? new Date().getFullYear() : new Date().getFullYear() - 1;
  const prevOfferings = [];

  // if the course was offered in the previous academic year, add the corresponding emoji to prevOfferings
  if (terms.includes(`${prevYear - 1} Fall`)) prevOfferings.push('🍂');
  if (terms.includes(`${prevYear} Winter`)) prevOfferings.push('❄️');
  if (terms.includes(`${prevYear} Spring`)) prevOfferings.push('🌸');
  if (terms.some((t) => t.startsWith(`${prevYear} Summer`))) prevOfferings.push('☀️');

  // if the course was not offered in the previous academic year, show a ❌
  if (prevOfferings.length === 0) prevOfferings.push('❌');

  const popover = (
    <Popover id="recent-offerings-popover" className="recent-offerings-popover">
      <div className="popover-body">
        <RecentOfferingsTable terms={terms} size="thin" />
      </div>
    </Popover>
  );

  return (
    <OverlayTrigger overlay={popover} placement="auto">
      <div className="tooltip-trigger">
        {prevOfferings.map((emoji) => (
          <span key={emoji}>{emoji}</span>
        ))}
      </div>
    </OverlayTrigger>
  );
};

export default RecentOfferingsTooltip;
