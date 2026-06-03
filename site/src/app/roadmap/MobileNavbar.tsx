import { FC } from "react";
import './MobileNavbar.scss';
import { Tabs } from "@mui/material";
import { Tab } from "@mui/material";
import SwapHorizOutlinedIcon from '@mui/icons-material/SwapHorizOutlined';
import FormatListBulletedIcon from '@mui/icons-material/FormatListBulleted';
import SearchIcon from '@mui/icons-material/Search';
import RouteIcon from '@mui/icons-material/Route';

interface MobileNavbarProps {
    selectedMobileIndex: number;
    onTabChange: (newValue: number) => void;
}

const MobileNavbar: FC<MobileNavbarProps> = ({ selectedMobileIndex, onTabChange }) => {
    const handleChange = (_event: React.SyntheticEvent, newValue: number) => {
        onTabChange(newValue);
    };

    return (
        <Tabs
            className="mobile-navbar"
            value={selectedMobileIndex}
            onChange={handleChange}
            variant="fullWidth"
            scrollButtons="auto"
            >
            <Tab icon={<RouteIcon/>} />
            <Tab icon={<SwapHorizOutlinedIcon/>} />
            <Tab icon={<FormatListBulletedIcon/>} />
            <Tab icon={<SearchIcon/>} />
        </Tabs>
    );
};

export default MobileNavbar;