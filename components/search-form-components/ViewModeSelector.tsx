import Paper from '@material-ui/core/Paper';
import DashboardIcon from '@material-ui/icons/Dashboard';
import LibraryIcon from '@material-ui/icons/ImportContacts';
import TableIcon from '@material-ui/icons/TableChart';
import ImagesIcon from '@material-ui/icons/ViewComfy';
import ToggleButton from '@material-ui/lab/ToggleButton';
import ToggleButtonGroup from '@material-ui/lab/ToggleButtonGroup';
import styled from 'styled-components';

interface ViewModeSelectorProps {
  showSubjectChangeSection?: boolean;
  viewMode: string;
  viewSubject: string;
  setViewSubject?: (subject: string) => void;
  setViewMode: (mode: string) => void;
}
const ViewModeSelector: React.FC<ViewModeSelectorProps> = ({
  showSubjectChangeSection = true,
  viewMode,
  viewSubject,
  setViewSubject,
  setViewMode,
}) => {
  const updateViewSubject = (viewSubjectValue) => {
    setViewSubject(viewSubjectValue);
  };

  const handleViewSubjectChange = (event: React.MouseEvent<HTMLElement>, newViewSubject: string | null) => {
    if (newViewSubject != null) {
      updateViewSubject(newViewSubject);
    }
  };

  const updateViewMode = (viewModeValue) => {
    setViewMode(viewModeValue);
  };

  const handleViewModeChange = (event: React.MouseEvent<HTMLElement>, newViewMode: string | null) => {
    if (newViewMode != null) {
      updateViewMode(newViewMode);
    }
  };

  return (
    <ToggleGroupWrapper>
      <div>
        {showSubjectChangeSection && (
          <StyledToggleButtonGroup value={viewSubject} onChange={handleViewSubjectChange} exclusive aria-label="view as cards or sets">
            <StyledToggleButton value="cards" aria-label="card">
              <DashboardIcon style={{ marginRight: '3px' }} /> View Cards
            </StyledToggleButton>
            <StyledToggleButton value="sets" aria-label="set">
              <LibraryIcon style={{ marginRight: '3px' }} /> View Sets
            </StyledToggleButton>
          </StyledToggleButtonGroup>
        )}
      </div>
      <div>
        <StyledToggleButtonGroup
          value={viewMode}
          onChange={handleViewModeChange}
          exclusive
          aria-label="view as grid or table"
          style={{ marginTop: '10px' }}
        >
          <StyledToggleButton value="grid" aria-label="grid">
            <ImagesIcon style={{ marginRight: '3px' }} /> As A Grid
          </StyledToggleButton>
          <StyledToggleButton value="table" aria-label="table">
            <TableIcon style={{ marginRight: '3px' }} /> As A Table
          </StyledToggleButton>
        </StyledToggleButtonGroup>
      </div>
    </ToggleGroupWrapper>
  );
};

const ToggleGroupWrapper = styled(Paper)(() => ({ margin: '10px', marginTop: '0px' }));
const StyledToggleButton = styled(ToggleButton)(() => ({ alignItems: 'end', justifyContent: 'center', width: '100%' }));
const StyledToggleButtonGroup = styled(ToggleButtonGroup)(() => ({ width: '100%' }));

export default ViewModeSelector;
