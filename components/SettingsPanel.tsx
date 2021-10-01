import FormControl from '@material-ui/core/FormControl';
import FormControlLabel from '@material-ui/core/FormControlLabel';
import FormGroup from '@material-ui/core/FormGroup';
import FormLabel from '@material-ui/core/FormLabel';
import IconButton from '@material-ui/core/IconButton';
import Popover from '@material-ui/core/Popover';
import Switch from '@material-ui/core/Switch';
import SettingsIcon from '@material-ui/icons/Settings';
import { useState } from 'react';
import styled from 'styled-components';

interface Setting {
  key: string;
  label: string;
  getToggleHiddenProps: any; // eslint-disable-line @typescript-eslint/no-explicit-any
}

export interface SettingGroup {
  label: string;
  settings: Setting[];
}

interface SettingsPanelProps {
  settingGroups: SettingGroup[];
  panelId: string;
}

const SettingsPanel: React.FC<SettingsPanelProps> = ({ settingGroups, panelId }) => {
  const [settingsPanelAnchorElement, setSettingsPanelAnchorElement] = useState<null | HTMLElement>(null);

  const handleSettingsButtonClick = (event: React.MouseEvent<HTMLButtonElement>) => {
    setSettingsPanelAnchorElement(event.currentTarget);
  };

  const handleCloseSettingsMenu = () => {
    setSettingsPanelAnchorElement(null);
  };

  return (
    <>
      <IconButton size="small" aria-controls={panelId} aria-haspopup="true" onClick={handleSettingsButtonClick}>
        <SettingsIcon color="disabled" />
      </IconButton>
      <Popover
        id={panelId}
        anchorEl={settingsPanelAnchorElement}
        keepMounted
        open={Boolean(settingsPanelAnchorElement)}
        onClose={handleCloseSettingsMenu}
        anchorOrigin={{
          vertical: 'bottom',
          horizontal: 'center',
        }}
        transformOrigin={{
          vertical: 'top',
          horizontal: 'center',
        }}
      >
        <SettingsPanelContentWrapper>
          <FormControl component="fieldset">
            {settingGroups.map((settingGroup) => (
              <FormGroup key={settingGroup.label}>
                <FormLabel component="legend" disabled>
                  {settingGroup.label}
                </FormLabel>
                {settingGroup.settings.map((setting) => (
                  <FormControlLabel
                    key={setting.key}
                    control={<Switch color="primary" checked={false} {...setting.getToggleHiddenProps()} />}
                    label={setting.label}
                  />
                ))}
              </FormGroup>
            ))}
          </FormControl>
        </SettingsPanelContentWrapper>
      </Popover>
    </>
  );
};

const SettingsPanelContentWrapper = styled.div(() => ({ margin: '10px', padding: '10px' }));

export default SettingsPanel;
