import FormControlLabel from '@material-ui/core/FormControlLabel';
import FormGroup from '@material-ui/core/FormGroup';
import Paper from '@material-ui/core/Paper';
import Switch from '@material-ui/core/Switch';
import Tooltip from '@material-ui/core/Tooltip';
import InfoOutlined from '@material-ui/icons/InfoOutlined';
import styled from 'styled-components';

interface IncludeSubsetGroupsToggleProps {
  includeSubsetGroups: boolean;
  updateIncludeSubsetGroups: (showAllPrintings: boolean) => void;
}

const IncludeSubsetGroupsToggle: React.FC<IncludeSubsetGroupsToggleProps> = ({ includeSubsetGroups, updateIncludeSubsetGroups }) => {
  const handleIncludeSubsetGroupsChange = (event: React.ChangeEvent<HTMLInputElement>) => {
    updateIncludeSubsetGroups(event.target.checked);
  };

  return (
    <ToggleGroupWrapper variant="outlined">
      <FormGroup>
        <StyledFormControlLabel
          control={
            <Switch checked={includeSubsetGroups} onChange={handleIncludeSubsetGroupsChange} name="includeSubsetGroups" color="primary" />
          }
          label={
            <div style={{ display: 'flex', alignItems: 'center', flexWrap: 'wrap' }}>
              Show Subset Groups
              <Tooltip
                placement="right"
                title={
                  <>
                    A subset group is a collection of related subsets.
                    <br /> <br />
                    Example: Prerelease Cards is a subset group. It contains subsets like Tempest Prerelease Promos, Urza's Saga Prerelease
                    Promos, and so on.
                    <br /> <br />
                    Subsets can be browsed either on their main set's page (you can see Tempest Prerelease Promos in the Tempest set page)
                    or in their subset group's page.
                    <br /> <br />
                    This setting is on by default.
                  </>
                }
              >
                <InfoOutlined color="disabled" style={{ marginLeft: '5px' }} />
              </Tooltip>
            </div>
          }
        />
      </FormGroup>
    </ToggleGroupWrapper>
  );
};

const ToggleGroupWrapper = styled(Paper)(() => ({ margin: '8px', marginTop: '0px' }));
const StyledFormControlLabel = styled(FormControlLabel)(() => ({ display: 'flex', justifyContent: 'center' }));

export default IncludeSubsetGroupsToggle;
