import { useState, FC } from "react";
import { useAppDispatch } from "../../store/hooks";
import './MobileNavbar.scss';
import { Tabs } from "@mui/material";
import { Tab } from "@mui/material";
import SwapHorizOutlinedIcon from '@mui/icons-material/SwapHorizOutlined';
import FormatListBulletedIcon from '@mui/icons-material/FormatListBulleted';
import SearchIcon from '@mui/icons-material/Search';
import RouteIcon from '@mui/icons-material/Route';
import { setSelectedMobileTab } from "../../store/slices/roadmapSlice";

interface MobileNavbarProps {
    selectedMobileIndex: number;
}

const MobileNavbar: FC<MobileNavbarProps> = ({ selectedMobileIndex }) => {
    const dispatch = useAppDispatch();
    const handleChange = (event: React.SyntheticEvent, newValue: number) => {
        dispatch(setSelectedMobileTab(newValue));
    };

    return (
        <Tabs
            className="mobile-navbar"
            value={selectedMobileIndex}
            onChange={handleChange}
            variant="fullWidth"
            scrollButtons="auto"
            >
            <Tab icon={<RouteIcon/>} label="Roadmap" />
            <Tab icon={<SwapHorizOutlinedIcon/>} label="Credits" />
            <Tab icon={<FormatListBulletedIcon/>} label="Catalog" />
            <Tab icon={<SearchIcon/>} label="Search" />
        </Tabs>
    );
};

export default MobileNavbar;